import Data.Bifunctor
import Data.List
import Data.Tuple.Extra ((&&&))

main = do
    fileContent <- readFile "./010/in.txt"
    let input = lines fileContent
    print $ one input
    print $ two input

solve scoring f1 f2 = scoring
                    . filter (uncurry (&&) . bimap f1 f2)
                    . fmap (go [])
  where go (s:ss) (x:xs) | close s x = go ss xs
        go s      (x:xs) | isOpen x  = go (x:s) xs
        go s      x                  = (s, x)
        isOpen                       = flip elem "{[(<"
        close s x                    = flipP s == x

flipP '{' = '}'
flipP '[' = ']'
flipP '(' = ')'
flipP '<' = '>'

two = solve scoring (not . null) null
  where scoring       = median
                      . fmap (foldl' ((+) . (5 *)) 0
                            . fmap (score . flipP)
                            . fst)
        score ')' = 1
        score ']' = 2
        score '}' = 3
        score '>' = 4
        median    = uncurry (!!) 
                  . second (`div` 2)
                  . (id &&& length)
                  . sort

one = solve scoring (not . null) (not . null)
  where scoring   = sum . fmap (score . head . snd)
        score ')' = 3
        score ']' = 57
        score '}' = 1197
        score '>' = 25137
