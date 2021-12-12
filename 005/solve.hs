{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}

import Data.List
import Data.List.Split
import Data.Tuple.Extra
import Control.Monad

main = do
    fileContent <- readFile "./005/in.txt"
    let input = parse <$> lines fileContent
    print $ one input
    print $ two input
      where parse = (\(a:b:_) -> (a, b))
                  . fmap (read @(Int, Int) . \a -> '(' : a ++ ")")
                  . splitOn " -> "

line diag ((a, b), (c, d)) | a == c    = (a,) <$> range b d
                           | b == d    = (,b) <$> range a c
                           | diag      = zip (range a c) (range b d)
                           | otherwise = []
    where range a b | a < b     = [a .. b]
                    | otherwise = reverse [b .. a]

solve diag = length 
           . filter (> 1) 
           . fmap snd 
           . counts
           . (line diag =<<)
    where counts = fmap (head &&& length) . group . sort

one = solve False
two = solve True

