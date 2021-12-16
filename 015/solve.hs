{-# LANGUAGE TupleSections #-}

import qualified Data.HashMap.Strict as M
import qualified Data.Set as S
import qualified Data.Heap as H
import Data.List
import Data.Char
import Data.Maybe
import Control.Arrow
import Debug.Trace

main = do
    fileContent <- readFile "./015/in.txt"
    let input = parse fileContent
    print $ one input
    print $ two input

parse = M.fromList . digit . inside . coords . fmap coords . lines
    where coords = zip [1 ..]
          inside = concatMap $ \(i, x) -> fmap (first (i,)) x
          digit  = fmap $ second $ subtract 48 . ord

one = findPath

two = findPath . augment

findPath graph = fst . fromJust $ bellmanford graph (1, 1) (max graph) 
    where max = maximum . M.keys

-- I know dijkstra is faster (in reality not that much) but I'm dumb
bellmanford graph start end = go S.empty (S.singleton (0, start))
    where go visited tovisit = case S.minView tovisit of
            Nothing                        -> Nothing
            Just ((cost, current), rest)
                | current == end           -> Just (cost, current)
                | S.member current visited -> go visited rest
                | otherwise                -> go visited' rest'
                where visited' = S.insert current visited
                      rest'    = foldr S.insert rest next
                      next     = fmap (first $ (+ cost ) . fromJust)
                               . filter (isJust . fst)
                               . fmap (flip M.lookup graph &&& id)
                               $ adj current

augment m = M.fromList . concatMap aug . M.toList $ m
  where wrapsize                 = maximum . fmap fst . M.keys $ m
        tiles                    = concatMap (zip [0 .. 4] . repeat) [0 .. 4]
        aug p                    = map p <$> tiles
        map ((x, y), r) (rx, ry) = ((x + wrapsize * rx, y + wrapsize * ry),
                                               wrap r rx ry)
        wrap r rx ry = (r + rx + ry - 1) `mod` 9 + 1

adj p = fmap (add p) [(-1 , 0), (1 , 0), (0 , -1), (0 , 1)]
  where add (a, b) (c, d) = (a + c, b + d)

                                     
