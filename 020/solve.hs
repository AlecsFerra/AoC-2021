{-# LANGUAGE TupleSections #-}

import Data.List.Split
import Control.Arrow
import qualified Data.HashSet as S
import Control.Applicative
import Numeric
import Data.Char
import Data.List
import Control.Monad
import Debug.Trace

main = do
    fileContent <- readFile "./020/in.txt"
    let (algo, image) = parse fileContent
    print $ one algo image
    print $ two algo image

parse inp = let [algo, image] = splitOn "\n\n" inp
             in (parseAlgo algo, (True, parseImage image))
            where parseImage = S.fromList
                             . fmap fst
                             . filter ((== '#') . snd)
                             . index2d
                             . lines
                  parseAlgo  = takehash . index
                  takehash   =  S.fromList
                             . fmap fst
                             . filter ((== '#') . snd)
                  index2d    = inside . index . fmap index
                  index      = zip [0 ..]
                  inside     = concatMap $ \(i, x) -> fmap (first (i,)) x

one = solve 2
two = solve 50

solve n algo = S.size . snd . (!! n) . iterate (enhance algo)

enhance alg (void, img) = (void', img')
  where void' | void      = S.member (511 :: Int) alg
              | otherwise = S.member (0   :: Int) alg
        points            = S.fromList $ concatMap adj img
        img'              = S.filter filter points
        filter            = (== void')
                          . (`S.member` alg)
                          . readBin
                          . fmap inimg
                          . adj
        inimg p           = if S.member p img == void then '1' else '0'


adj (x, y) = (,) <$> [x - 1 .. x + 1] <*> [y - 1 .. y + 1] 

readBin = fst . head . readInt 2 (`elem` "01") digitToInt
