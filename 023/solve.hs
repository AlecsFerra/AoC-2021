{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import qualified Data.IntMap.Strict as IM
import Data.Char
import Data.Maybe
import Data.List hiding (null, singleton, insert)
import Prelude hiding (null)
import Control.Monad
import Control.Arrow
import Debug.Trace

main = do
    fileContent <- readFile "./023/in.txt"
    let inputOne = parse fileContent
    let inputTwo = parse $ expand fileContent
    print $ one inputOne
    --render inputTwo
    print $ two inputTwo

parse = M.fromList . inside . coords . fmap coords . lines
    where coords = zip [0 :: Int ..]
          inside = concatMap $ \(i, x) -> fmap (first (i,)) x

-- Oh mi sono rotto il cazzo
expand input = let [a, b, c, d, e] = lines input
                in intercalate "\n" [a, b, c, l1, l2, d, e]
                where l1 = "  #D#C#B#A#"
                      l2 = "  #D#B#A#C#"

one = solve
two = solve

steps world = do
  (start, fish) <- M.toList world -- Check every cell
  guard $ fish `elem` "ABCD" -- Is this cell really a fish?
  destination   <- paths world start -- Where the fish can go
  guard $ validDestination world fish start destination -- Is destination valid?
  let world' = M.insert start       '.'
             $ M.insert destination fish world
  let cost   = moveCost fish * norm start destination
  pure (cost, world')

  where norm (x, y) (x', y') = abs (x - x') + abs (y - y')
        moveCost             =  (10 ^) . subtract (ord 'A') . ord

validDestination world fish start to | room start =  hallway to
                                     | otherwise  =  room to
                                                  && snd to == roomOf fish
                                                  && clean world fish
                                                  && bottom world to fish
  where hallway              = (== (True, True)) . (((== 1) . fst) &&& (not . room))
        room (_, y)          = odd y && y /= 1 && y /= 11
        clean world fish     = all (`elem` [fish, '.'])
                             $ takeWhile (/= '#')
                             $ fmap ((world M.!) . (,roomOf fish)) [2 ..]
        bottom world to fish = world M.! first (+ 1) to `elem` ['#', fish]

roomOf = (+ 3) . (* 2) . subtract (ord 'A') . ord

paths world = dfs S.empty . adj
  where dfs _ []                            = []
        dfs seen (x:xs) | x `S.member` seen = dfs seen xs
                        | not $ valid x     = dfs seen xs
                        | otherwise         = x : dfs (x `S.insert` seen)
                                                      (adj x ++ xs)
        valid                               = (== Just '.') . (world M.!?)
        adj (x, y)                          = [ (x, y + 1)
                                              , (x - 1, y)
                                              , (x + 1, y)
                                              , (x, y - 1) ]

complete world = and $ fmap completeLine "ABCD"
  where completeLine fish = all (== fish)
                          $ takeWhile (/= '#')
                          $ fmap ((world M.!) . (,roomOf fish)) [2 ..]

solve world = dijkstra S.empty (singleton 0 world)
  where dijkstra _ Empty      = error "TF BRUH"
        dijkstra seen queue
          | x `S.member` seen = dijkstra seen rest
          | complete x        = cost
          | otherwise         = dijkstra (x `S.insert` seen) rest'
          where (cost, x, rest) = fromJust $ viewWithPriority queue
                rest'           = foldl' merge rest (steps x)
                merge q (c, w)  = insert (c + cost) w q

-- PQueue :) thanks xerox
newtype PQueue a = PQ (IM.IntMap [a])

pattern Empty :: PQueue a
pattern Empty <- (null -> True)
  where
    Empty = PQ IM.empty

insert :: Int -> a -> PQueue a -> PQueue a
insert k v (PQ m) = PQ (IM.alter f k m)
  where
    f Nothing   = Just [v]
    f (Just vs) = Just (v:vs)

singleton :: Int -> a -> PQueue a
singleton p v = PQ (IM.singleton p [v])

null :: PQueue a -> Bool
null (PQ m) = IM.null m

viewWithPriority :: PQueue a -> Maybe (Int,a,PQueue a)
viewWithPriority (PQ m) =
  do ((p,vs),m') <- IM.minViewWithKey m
     case vs of
      [] -> error "malformed queue"
      [v] -> Just (p,v,PQ m')
      (v:vs) -> p' `seq` Just (p,v,p')
        where
          p' = PQ (IM.insert p vs m')

--Debug is kool guys :)
render s = let maxX = maximum $ fst <$> M.keys s
               maxY = maximum $ snd <$> M.keys s
            in forM_ [0 :: Int .. maxX] $ \x -> do
                forM_ [0 :: Int .. maxY] $ \y ->
                    putChar $ M.findWithDefault ' ' (x, y) s
                putChar '\n'
