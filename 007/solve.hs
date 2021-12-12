import Data.List
import Data.Bifunctor
import Data.Tuple.Extra ((&&&))

main = do
    fileContent <- readFile "./007/in.txt"
    let input = read $ "[" ++ fileContent ++ "]"
    print $ one input
    print $ two input

solve s d x = sum $ fmap (d . abs . subtract (s x)) x

one = solve median id
    where median = uncurry (!!) 
                 . second (`div` 2)
                 . (id &&& length)
                 . sort

two = solve meanP distance
    where distance = flip div 2 . uncurry (*) . (id &&& (1 +))
          meanP    = floor -- sometimes ceiling, sometimes floor
                   . uncurry (/)
                   . bimap fromIntegral fromIntegral
                   . (sum &&& length)
