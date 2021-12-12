import qualified Data.Map as M
import qualified Data.Set as S
import Data.List.Split
import Data.Bifunctor
import Data.List
import Data.Char
import Data.Maybe

main = do
    fileContent <- readFile "./012/in.txt"
    let adj = parse $ lines fileContent
    print $ one adj
    print $ two adj

parse = foldl' insertion M.empty . concatMap (fmap (second return) . parseLine)
  where parseLine          = take . splitOn "-"
        take [a, b]        = [(a, b), (b, a)]
        insertion m (k, v) = M.insertWith (++) k v m

one adj = solve adj False
two adj = solve adj True

solve adj = length . paths adj "start" "end"

paths g f t twice = dfs f t S.empty twice
    where dfs f t v twice | f == t = [[t]] -- Saving the paths for debug (slow :))
          dfs f t v twice          = fmap (f :)
                                   . concatMap (step t (S.insert f v))
                                   . mapMaybe (next v twice)
                                   $ g M.! f

          next v _ f     | startEnd f && S.member f v = Nothing
          next v twice f | startEnd f                 = Just (f, twice)
          next v False f | small f && S.member f v    = Nothing
          next v True  f | small f && S.member f v    = Just (f, False)
          next _ twice f                              = Just (f, twice)

          startEnd "start" = True
          startEnd "end"   = True
          startEnd _       = False

          small                    = all isLower
          step t v (f, twice)      = dfs f t v twice
