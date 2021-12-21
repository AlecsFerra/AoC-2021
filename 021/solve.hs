import Debug.Trace
import Control.Monad
import Data.List
import Data.MemoUgly

main = do
    fileContent <- readFile "./021/test.txt"
    let [start1, start2] = fmap (read .  last . words) $ lines $ fileContent
    print $ one start1 start2
    print $ two start1 start2


wrap x = (x - 1) `mod` 10 + 1

one p1 p2 = go 0 (p1, 0) (p2, 0)
      where go turns (p1, s1) (p2, s2) | s1' >= 1000 = 3 * turns' * s2
                                       | otherwise   = go turns' (p2,  s2)
                                                                 (p1', s1')
                  where p1'    = wrap (p1 + 6 - turns)
                        s1'    = s1 + p1'
                        turns' = turns + 1

two p1 p2 = states (p1, 0) (p2, 0)

states = memo states'
states' m1@(_, s1) m2@(_, s2) | s2 >= 21  = (0, 1)
                              | s1 >= 21  = (1, 0)
                              | otherwise = sump
                                          $ fmap (tick m1 m2) possiblerolls
    where possiblerolls = sum <$> replicateM 3 [1..3]
          sump          = foldl' (\(a, b) (c, d) -> (a + c, b + d)) (0, 0)
          tick (p1, s1) p2 roll = states p2 (p1', s1')
              where p1'         = wrap (p1 + roll)
                    s1'         = s1 + p1'


