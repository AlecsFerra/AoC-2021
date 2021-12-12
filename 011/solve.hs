{-# LANGUAGE TupleSections #-}

import Data.List
import qualified Data.Map as M
import Data.Bifunctor
import Data.Char
import Data.Tuple.Extra ((&&&))
import Data.Maybe

main = do
    fileContent <- readFile "./011/in.txt"
    let input = M.fromList . parse $ lines fileContent
    print $ one 100 input
    print $ two input

one steps = length . filter (== 0) . concat . take (steps + 1) . solve

two = findJust (all (== 0)) . solve
  where findJust = fromJust .: findIndex
        (.:) = (.) . (.)

solve = fmap M.elems . iterate step

parse = fmap (second digitToInt) . compact . coord . fmap coord
    where compact = foldl' (\a (i, x) -> a ++ fmap (first (i,)) x) []
          coord = zip [0..]

step = uncurry (foldl' flash) . (fmap (+1) &&& goingtoflash)
  where goingtoflash = fmap fst . filter ((== 9) . snd) . M.toList

flash m o = case M.lookup o m of
              Just x | x >= 9 -> foldl' flash (M.insert o 0 m) (adj o)
                     | x >= 1 -> M.insert o (x + 1) m
              _               -> m

adj p = fmap (add p) [(-1,  1), (0,  1), (1,  1),
                      (-1,  0),          (1,  0),
                      (-1, -1), (0, -1), (1, -1)]
  where add (a, b) (c, d) = (a + c, b + d)
