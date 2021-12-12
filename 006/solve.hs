import Data.List
import Data.Tuple.Extra

main = do
    fileContent <- readFile "./006/test.txt"
    let input = read $ "[" ++ fileContent ++ "]"
    let compressed = prepare input
    print compressed
    print $ one compressed
    print $ two compressed

x      // []            = x
(x:xs) // ((0, x'):xs') = x' : xs // si xs'
(x:xs) // xs'           = x  : xs // si xs'

si = fmap $ first $ subtract 1

prepare = (replicate 9 0 //) . counts
    where counts = fmap (head &&& length) . group . sort 


solve 0 a = sum a
solve n [a0, a1, a2, a3, a4, a5, a6, a7, a8] = solve (n - 1) a'
    where a' = [a1, a2, a3, a4, a5, a6, a7 + a0, a8, a0]

one = solve 80
two = solve 10000
