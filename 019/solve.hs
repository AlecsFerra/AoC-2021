{-# LANGUAGE TupleSections #-}
import Data.List.Split
import qualified Data.Set as S
import qualified Data.Map as M
import Debug.Trace
import Data.List
import Control.Arrow
import Control.Monad
import Data.Maybe
import Data.Either

orient = transpose . fmap (orientation >=> spin)
      where orientation (x, y, z) =
                  [ (x, y, z) 
                  , (y, - x, z)
                  , (y, z, x)
                  , (- y, x, z)
                  , (- x, - y, z)
                  , (y, - z, - x)
                  ]
            spin (x, y, z) =
                  [ (x, y, z)
                  , (x, - y, - z)
                  , (x, - z, y)
                  , (x, z, - y)
                  ]

main = do
    fileContent <- readFile "./019/in.txt"
    let input = parse fileContent
    print $ one input
    print $ two input

parse = fmap parseScanner . splitOn "\n\n"
      where parseScanner = fmap parsePoint . tail . lines
            parsePoint x = read $ "(" ++ x ++ ")"

adjust fixed trans = case adjust' fixed trans of
                       Just p  -> Left p
                       Nothing -> Right trans
      where adjust' fixed trans = listToMaybe $ do
            trans <- orient trans -- picks a possible orientation
            tp    <- trans
            fp    <- fixed
            let origin = fp `sub` tp
            let trans' = fmap (`add` origin) trans
            let fixeds = S.fromList fixed
            guard $ length (filter (`S.member` fixeds) trans') >= 12 
            pure (trans', origin)


add (x, y, z) (x', y', z') = (x + x', y + y', z + z')
sub (x, y, z) (x', y', z') = (x - x', y - y', z - z')

one = length . nub . concat . map fst . solve
two = maximum . map (uncurry mdistance) . pick2 . map snd . solve


pick2 [] = []
pick2 (x:xs) = map (x ,) xs ++ pick2 xs

solve (x:xs) = go [(x, (0, 0, 0))] [x] xs
      where go result _ []                = result
            go result (ref:refs) rest = go (found ++ result)
                                           (map fst found ++ refs)
                                           notFound
                  where (found, notFound) = partitionEithers
                                          $ fmap (adjust ref) rest

mdistance (x, y, z) (x', y', z') = abs (x - x') -- fuck real numbers
                                 + abs (y - y') -- all my homies like
                                 + abs (z - z') -- Manhattan
