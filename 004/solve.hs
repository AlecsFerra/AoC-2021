{-# LANGUAGE TypeApplications #-}

import Data.Maybe
import Data.List
import Data.List.Split

main = do
  fileContent <- readFile "./004/in.txt"
  let (nums, boards) = parse fileContent
  let game           = runGame nums boards 
  print $ one game
  print $ two game

parse s = let (nums:_:boards) = lines s
          in (parseNums nums, parseBoards boards)
            where parseBoards = chunksOf 5
                              . fmap (fmap (Just . read @Int) . words)
                              . filter (/= "")
                  parseNums   = fmap read . splitOn ","


mark x = (fmap . fmap) (delete (x ==))
  where delete p j@(Just a) | p a       = Nothing
                            | otherwise = j
        delete _ _                      = Nothing

win board = check board || check (transpose board)
  where check = any (all (== Nothing))

runGame []     _          = []
runGame (x:xs) boards     = fmap computeScore winning ++ runGame xs losing
  where (winning, losing) = partition win (fmap (mark x) boards)
        computeScore      = (* x) . sum . fmap (sum . catMaybes)

one = head
two = last
