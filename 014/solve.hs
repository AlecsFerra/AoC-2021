import qualified Data.Map.Strict as M
import Data.List.Split
import Control.Arrow
import Data.List

main = do
    fileContent <- readFile "./014/big.txt"
    let (polymer, rules) = parse fileContent
    print $ one rules polymer
    print $ two rules polymer 
    print $ big rules polymer 

parse input = let [polymer, rules] = splitOn "\n\n" input
               in (compress polymer,
                   M.fromList $ parseRule . words <$> lines rules)
    where parseRule [[fst, snd], _, [to]] = ((fst, snd), to)
          compress                        = counting . window . (' ' :)
          window                          = uncurry zip . (id &&& drop 1)
          counting                        = foldl' insert M.empty
          insert m e                      = M.insertWith (+) e 1 m

apply rules polymer = M.fromListWith (+) $ concatMap step $ M.toList polymer
    where step (pair@(l, r), count) = case rules M.!? pair of
                                        Just mid -> [((l, mid), count),
                                                     ((mid, r), count)]
                                        Nothing  -> [(pair, count)]

one = solve 10
two = solve 40
big = solve 1000000

solve n rules polymer = op $ iterate (apply rules) polymer !! n
    where op = uncurry (-)
             . (maximum &&& minimum)
             . fmap snd 
             . M.toList 
             . M.mapKeysWith (+) snd

