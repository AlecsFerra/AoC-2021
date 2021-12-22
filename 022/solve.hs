{-# LANGUAGE TupleSections #-}

import Data.Tuple.Extra
import Data.List.Split
import Data.Maybe
import Data.List hiding (intersect)

data S  = On | Off
neg On  = Off
neg Off = On

main = do
    fileContent <- readFile "./022/in.txt"
    let input = cube <$> parse fileContent
    print $ one input
    print $ two input

one = solve . filter (isJust . intersect bound . snd)
    where bound = ((-50, -50, -50), (50, 50, 50))

two = solve

solve = totalVolume . merge

intersect (a1, a2) (b1, b2) = do
    (x1, x2) <- (fst3 a1, fst3 a2) `intersect'` (fst3 b1, fst3 b2)
    (y1, y2) <- (snd3 a1, snd3 a2) `intersect'` (snd3 b1, snd3 b2)
    (z1, z2) <- (thd3 a1, thd3 a2) `intersect'` (thd3 b1, thd3 b2)
    return ((x1, y1, z1), (x2, y2, z2))
    where intersect' (a1, a2) (b1, b2)
            | b1 > a2 || a1 > b2 = Nothing
            | otherwise          = Just (max a1 b1, min a2 b2)


totalVolume = sum . map (uncurry (*) . first volume)
    where volume = uncurry3 ((*) .: (*))
                 . uncurry (trimap $ flip (-) . subtract 1)

merge = foldl' merge' []
    where merge' m (On,  r)   = m `add` r
          merge' m (Off, r)   = m `sub` r
          m `sub` r           = mapMaybe (intersect' r) m ++ m
          m `add` r           = (r, 1) : (m `sub` r)
          intersect' r (s, m) = (, negate m) <$> (s `intersect` r)

-- Shhh
parse = fmap (parseL . words) . lines
      where parseL ["on",  rest] = (On,  parseS rest)
            parseL ["off", rest] = (Off, parseS rest)
            parseS i = let [xr, yr, zr] = splitOn "," i
                        in (parseR xr, parseR yr, parseR zr)
            parseR i = let [bt, tp] = splitOn ".." i
                        in (read $ drop 2 bt, read tp)
(.:) = (.) . (.)
infixr 0 .:

trimap f (x, y, z) (x', y', z')  = (f x x', f y y', f z z')

cube (s, ((xmin, xmax), (ymin, ymax), (zmin, zmax))) =
      (s, ((xmin, ymin, zmin), (xmax, ymax, zmax)))
