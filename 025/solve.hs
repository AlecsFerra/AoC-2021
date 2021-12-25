{-# LANGUAGE TupleSections #-}

import qualified Data.Map as M
import Control.Arrow
import Control.Monad
import Data.List
import Debug.Trace

main = do
    fileContent <- readFile "./025/in.txt"
    let (input, max) = parse fileContent
    print $ one max input
    print $ two max input

parse = (M.fromList . filter ((/= '.') . snd) &&& maximum . fmap fst)
      . inside
      . coords
      . fmap coords
      . lines
    where coords = zip [0 ..]
          inside = concatMap $ \(i, x) -> fmap (first (i,)) x

one = solve
two = const $ const "Buon Natale!"

solve s world = go 1 s world
    where go n s world | world == world' = n
                       | otherwise       = go (n + 1) s world'
                    where world' = step s world

step (maxX, maxY) = step' 'v' . step' '>'
          where step' e m = M.fromList $ do
                    (p, d) <- M.toList m
                    let p' =  move d p
                    if d == e && M.notMember p' m
                        then pure (p', d)
                        else pure (p,  d)

                move 'v' (x, y) = wrap (x + 1, y)
                move '>' (x, y) = wrap (x, y + 1)
                wrap (x, y)     = (x `mod` (maxX + 1), y `mod` (maxY + 1))

render s (maxX, maxY) = forM_ [0 .. maxX] $ \x -> do
                            forM_ [0 .. maxY] $ \y ->
                                putChar $ M.findWithDefault '.' (x, y) s
                            putChar '\n'
