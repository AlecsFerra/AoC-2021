import Data.List.Extra

main = do
    fileContent <- readFile "./017/in.txt"
    let input = parse fileContent 
    print $ one input
    print $ two input

parse s = let [rangex, rangey] = drop 2 $ words s
           in (parserange $ init rangex, parserange rangey)
          where parserange s = let [min, max] = splitOn ".." $ drop 2 s
                                in (read min, read max)

one (_, (ymin, _)) = (-ymin * (-ymin - 1)) `div` 2

two box@((_, xmax), (ymin, _)) = length
                               $ filter id
                               $ fmap (simulate box) shots
    where shots = concatMap (zip [0 .. xmax + 1] . repeat) [ymin .. -ymin - 1]

simulate ((xmin, xmax), (ymin, ymax)) = go (0, 0) 
  where go (x, y) (xv, yv) | y < ymin || x > xmax = False
                           | xmin <= x,
                             x <= xmax,
                             ymin <= y,
                             y <= ymax            = True
                           | otherwise            = go (x', y') (xv', yv')
                           where x'  = x + xv
                                 y'  = y + yv
                                 xv' = xv - signum xv
                                 yv' = yv - 1

