import Data.List
import Control.Arrow
import Control.Applicative
import Data.Char
import Numeric
import Debug.Trace
import Data.Maybe
import Control.Monad

main = do
    fileContent <- readFile "./018/in.txt"
    let input = parse fileContent
    print $ one input
    print $ two input

parse = fmap (fst . fromJust . run number) . lines

one = magnitude . uncurry (foldl' (-+-)) . (head &&& tail)

two ns = maximum
       $ fmap (uncurry max . (magnitude . add' &&& magnitude . add)) pairs
      where pairs = concatMap (zip ns . repeat) ns
            add   = uncurry (-+-)
            add'  = uncurry $ flip (-+-)

magnitude (S n)    = n
magnitude (l :| r) = 2 * magnitude r + 3 * magnitude l

data N = S Int
       | N :| N
      deriving Show

n -+- m = reduce $ n :| m
      where reduce n = maybe n reduce (explode n <|> split n)

explode = go 4 Top
      where go 0 z (S l :| S r) = Just $ fill (S 0) (addUpL l (addUpR r z))
            go 0 _ _            = Nothing
            go n z (l   :|   r) =  go (n - 1) (L r z) l
                               <|> go (n - 1) (R l z) r
            go _ _ _            = Nothing

split (S n) | n >= 10   = Just $ S (n `div` 2) :| S ((n + 1) `div` 2)
            | otherwise = Nothing
split (l :| r)          =  (:| r) <$> split l
                       <|> (l :|) <$> split r

-- https://wiki.haskell.org/Zipper
-- OP
data NZip = Top
          | R N NZip
          | L N NZip
          deriving Show

fill with (R l z) = fill (l :| with) z
fill with (L r z) = fill (with :| r) z
fill with Top     = with

addUpL _ Top = Top
addUpL n (R l z) = R (addDownR n l) z
addUpL n (L r z) = L r (addUpL n z)

addUpR _ Top = Top
addUpR n (R l z) = R l (addUpR n z)
addUpR n (L r z) = L (addDownL n r) z

addDownL n (l :| r) = addDownL n l :| r
addDownL n (S m)    = S (n+m)

addDownR n (l :| r) = l :| addDownR n r
addDownR n (S m)    = S (n+m)

number :: Parser N
number = single <|> pair

pair :: Parser N
pair = Parser $ \rest -> do
      (_, rest)   <- run (expect '[') rest
      (fst, rest) <- run number rest
      (_, rest)   <- run (expect ',') rest
      (snd, rest) <- run number rest
      (_, rest)   <- run (expect ']') rest
      Just (fst :| snd, rest)

int :: Parser Int
int = Parser $ listToMaybe . readInt 10 (`elem` "1234567890") digitToInt

single :: Parser N
single = Parser $ \rest -> do
      (n, rest) <- run int rest
      Just (S n, rest)

expect n = Parser $ \rest -> do
      let x:xs = rest
      if x == n then Just (x, xs)
                else Nothing

newtype Parser a = Parser { run :: String -> Maybe (a, String) }

instance Functor Parser where
  fmap f (Parser p) =
    Parser $ \val -> do
          (parsed, rest) <- p val
          Just (f parsed, rest)

instance Applicative Parser where
  pure x = Parser $ \rest -> Just (x, rest)

  (Parser p1) <*> (Parser p2) =
    Parser $ \val -> do
      (f, rest) <- p1 val
      (v, rest) <- p2 rest
      Just (f v, rest)

instance Alternative Parser where
      empty = Parser $ const Nothing
      (Parser p1) <|> (Parser p2) =
            Parser $ uncurry f . (p1 &&& p2)
                  where f r@(Just _) _ = r
                        f _ l = l
