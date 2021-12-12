{-# LANGUAGE TupleSections #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}

import qualified Data.HashMap.Strict as M
import qualified Data.Set as S
import Data.Strict.Tuple hiding (zip)
import Data.Bifunctor
import Data.List
import Data.Char
import Data.Maybe
import Data.Ord
  
main = do
    fileContent <- readFile "./9-4096-2.in"
    let input = M.fromList . parse $ lines fileContent
    print $ one input
    print $ two input


parse = fmap (second digitToInt)
      . compact
      . coord
      . fmap coord
    where compact = foldl' (\a (i, x) -> a ++ fmap (first (i,)) x) []
          coord = zip [0 :: Int ..]

one m = sum . fmap succ . M.elems . lowPoints $ m

two m = product
      . take 3
      . sortOn Down
      . fmap (S.size . extend m)
      . M.keys $ lowPoints m

extend m p = flex S.empty [p]
  where flex v []     = v
        flex v (x:xs) | M.lookup x m == Just 9   = flex v xs
                      | isNothing $ M.lookup x m = flex v xs
                      | S.member x v             = flex v xs
                      | otherwise                = flex (S.insert x v)
                                                       (adj x ++ xs)

lowPoints m = M.filterWithKey (\p v -> all (v <) (adjv p)) m
  where adjv  = mapMaybe (`M.lookup` m) . adj

adj p = fmap (add p) [(-1 , 0), (1 , 0), (0 , -1), (0 , 1)]
  where add (a, b) (c, d) = (a + c, b + d)

