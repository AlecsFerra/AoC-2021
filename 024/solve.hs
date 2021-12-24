{-# LANGUAGE TypeApplications #-}

import Data.List.Split
import Data.Char

main = do
    fileContent <- readFile "./024/in.txt"
    let input = parse fileContent
    print $ one input
    print $ two input

parse = fmap extractChunk . chunksOf 18 . lines
    where extractChunk s = (extractLine (s !! 4),
                            extractLine (s !! 5),
                            extractLine (s !! 15))
          extractLine = read @Int . (!! 2) . words

one = map intToDigit . head . solve (reverse [1..9]) 0
two = map intToDigit . head . solve [1..9]           0

solve _       0 []                 = [[]]
solve _       _ []                 = []
solve guesses z ((a, b, c) : rest) = solved
    where solved = do
            current <- if b < 0 then
                           [w | let w = z`mod`26 + b, 1 <= w, w <= 9]
                        else
                           guesses
            left    <- solve guesses (performCycle a b c current z) rest
            pure (current : left)

performCycle a b c w z | z `mod` 26 + b == w = z `div` a
                       | otherwise           = z `div` a * 26 + w + c
