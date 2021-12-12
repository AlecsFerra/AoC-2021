import qualified Data.Map as M
import Data.List.Split
import Data.List
import Data.Maybe
import Data.Bifunctor

main = do
    fileContent <- readFile "./008/in.txt"
    let input = parseLine <$> lines fileContent
    print $ one input
    print $ two input
        where parseLine    = bimap (filter (/= ' ')) words
                           . pair
                           . splitOn " | " 
              pair (x:y:_) = (x, y)

solve t = sum . fmap (t . decode)

decode (patterns, nums) = fmap (fromId . ids) nums
    where histogram  = foldl insert M.empty patterns
          insert m k = M.insertWith (+) k 1 m
          ids        = sum . fmap (histogram M.!)
          fromId     = fromJust . (`elemIndex`
                     [42, 17, 34, 39, 30, 37, 41, 25, 49, 45, 9])

one = solve (length . filter (`elem` [1, 4, 7, 8]))

two = solve (read . concatMap show)
