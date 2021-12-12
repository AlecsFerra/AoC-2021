import Data.List
import Data.Char
import Numeric

main = do
    fileContent <- readFile "./003/in.txt"
    let input = lines fileContent
    print $ one input
    print $ two input

mPresent x = if many0 > many1 then '1' else '0'
    where many c = length $ filter (== c) x
          many0  = many '0'
          many1  = many '1'

lPresent = neg . mPresent
    where neg '0' = '1'
          neg '1' = '0'

one input = let parsed = transpose input
                γ      = map mPresent parsed
                ϵ      = map lPresent parsed
            in readBin γ * readBin ϵ 

two input = let o₂  = iterativeFilter 0 mPresent input
                co₂ = iterativeFilter 0 lPresent input
            in readBin o₂ * readBin co₂
    where iterativeFilter _ _ [x] = x
          iterativeFilter n p xs  = iterativeFilter (1 + n) p xs'
              where selected = p (map (!! n) xs)
                    xs'      = filter (\a -> selected == a !! n) xs


readBin = fst . head . readInt 2 (`elem` "01") digitToInt

