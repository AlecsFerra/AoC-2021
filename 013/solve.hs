{-# LANGUAGE LambdaCase #-}

import Data.List.Split
import Data.Bifunctor
import qualified Data.Set as S
import Control.Monad

(.:) = (.) . (.)
infixr .:

main = do
    fileContent <- readFile "./013/in.txt"
    let (points, folds) = first S.fromList $ parse fileContent
    print $ one points folds
    two points folds

data F = U | L
    deriving (Eq, Ord)

parse input = let [points, folds] = splitOn "\n\n" input
               in (parsePoint        <$> lines points,
                   parseFold . words <$> lines folds)
                       where parsePoint x              = read $ "(" ++ x ++ ")"
                             parseFold [_, _, 'x':_:n] = (L, read n)
                             parseFold [_, _, 'y':_:n] = (U, read n)

fold (U, n) = S.map $ \case (x, y) | y > n -> (x, 2 * n - y)
                            p              -> p
fold (L, n) = S.map $ \case (x, y) | x > n -> (2 * n - x, y)
                            p              -> p

solve = scanl $ flip fold

one = S.size .: (!! 1) .: solve

two = render .: last .: solve 

render s = let maxX = maximum $ S.map fst s
               maxY = maximum $ S.map snd s
            in forM_ [0 .. maxY] $ \y -> do
                forM_ [0 .. maxX] $ \x ->
                    putChar $ if S.member (x, y) s then 'X' else ' '
                putChar '\n'

