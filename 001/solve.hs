
main :: IO ()
main = do
    fileContent <- readFile "./001/in.txt"
    let input = fmap read $ lines fileContent 
    print $ one input
    print $ two input

general n x = length
            $ filter id
            $ uncurry (>)
           <$> zip (drop n x) x

one = general 1
two = general 3
