import Data.List
import Data.Bifunctor

main = do
    fileContent <- readFile "./002/in.txt"
    let input = fmap parse $ lines fileContent 
    print $ one input
    print $ two input

parse s = case words s of
    ["forward", step] -> (F, read step)
    ["down", step] -> (D, read step)
    ["up", step] -> (U, read step)


data Command = F | D | U
    deriving Show

solve i a  e = uncurry (*) . i . foldl' (flip a) e

one = solve id apply (0, 0)
    where apply (F, n) p = first (+ n) p
          apply (D, n) p = second (+ n) p
          apply (U, n) p = second (subtract n) p

two = solve snd apply (0, (0, 0))
    where apply (F, n) (a, p) = (a, bimap (+ n) (+ (a * n)) p)
          apply (D, n) p = first (+ n) p
          apply (U, n) p = first (subtract n) p
