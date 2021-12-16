import Data.List
import Control.Arrow
import Control.Applicative
import Data.Char
import Numeric
import Debug.Trace
import Data.Maybe
import Control.Monad

main = do
    fileContent <- readFile "./016/in.txt"
    let input = bin . head . lines $ fileContent
    print $ one input
    print $ two input

data Content = Literal Int
             | Operation [Packet]
             deriving Show

type Packet = (Int, Int, Content)

bin = concatMap tobin
    where tobin '0' = "0000"
          tobin '1' = "0001"
          tobin '2' = "0010"
          tobin '3' = "0011"
          tobin '4' = "0100"
          tobin '5' = "0101"
          tobin '6' = "0110"
          tobin '7' = "0111"
          tobin '8' = "1000"
          tobin '9' = "1001"
          tobin 'A' = "1010"
          tobin 'B' = "1011"
          tobin 'C' = "1100"
          tobin 'D' = "1101"
          tobin 'E' = "1110"
          tobin 'F' = "1111"

-- 2 kool 4 traversable
one = sumVersion . parse
      where sumVersion (v, _, Literal _)      = v
            sumVersion (v, _, Operation subs) = sum (fmap sumVersion subs) + v

two = eval . parse

eval (_, _, Literal it)     = it
eval (_, 0, Operation subs) = sum $ evalm subs
eval (_, 1, Operation subs) = product $ evalm subs
eval (_, 2, Operation subs) = minimum $ evalm subs
eval (_, 3, Operation subs) = maximum $ evalm subs
eval (_, 5, Operation subs) = trans $ eval (subs !! 0) > eval (subs !! 1)
eval (_, 6, Operation subs) = trans $ eval (subs !! 0) < eval (subs !! 1)
eval (_, 7, Operation subs) = trans $ eval (subs !! 0) == eval (subs !! 1)

evalm = fmap eval
trans True = 1
trans _    = 0

parse = fst . fromJust . run packet

packet = Parser $ \rest -> do
      (version, rest) <- run (b 3) rest
      (pid, rest)     <- run (b 3) rest
      (content, rest) <- if pid == 4
                              then run literal rest
                              else run operator rest
      Just ((version, pid, content), rest)

b n = Parser $ \rest -> do
      (s, rest) <- run (r n) rest
      i <- readbin s
      Just (i, rest)

r n = Parser $ Just . (take n &&& drop n)


operator = Parser $ \rest -> do
      (lid,       rest) <- run (b 1) rest
      if lid == 0 then do
            (len,       rest) <- run (b 15) rest
            (packetdom, rest) <- run (r len) rest
            (packets, _)      <- run (many packet) packetdom
            Just (Operation packets, rest)
      else do
            (npacks, rest) <- run (b 11) rest
            (packets, rest) <- run (replicateM npacks packet) rest
            Just (Operation packets, rest)

literal = Parser $ \rest -> do
      (start, rest) <- run (many $ cont 1) rest
      (last,  rest) <- run (cont 0) rest
      Just (Literal . fromJust . readbin $ concat start ++ last, rest)

cont c = Parser $ \rest -> do
      (l, rest) <- run (b 1) rest
      (n, rest) <- run (r 4) rest
      if l == c then Just (n, rest)
                else Nothing
           
readbin = fmap fst . listToMaybe . readInt 2 (`elem` "01") digitToInt

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
