import Control.Monad.State

-- A Very cool (but useless) implementation

data Register = W | X | Y | Z
    deriving Show

data Argument = R Register | N Int
    deriving Show

data Instruction = Inp Register
                 | Add Register Argument
                 | Mul Register Argument
                 | Div Register Argument
                 | Mod Register Argument
                 | Eql Register Argument
                deriving Show

data ALU =  ALU { w           :: Int
                , x           :: Int
                , y           :: Int
                , z           :: Int
                , inputStream :: [Int]
                , program     :: [Instruction]
                }
    deriving Show

runProgram :: [Instruction] -> [Int] -> ALU
runProgram ins inp = execState run alu
    where alu = ALU { w = 0
                    , x = 0
                    , y = 0
                    , z = 0
                    , inputStream = inp
                    , program = ins
                    }

run :: State ALU ()
run = do
    e <- ended
    if e then
        pure ()
    else
        Main.cycle

cycle :: State ALU ()
cycle = do
    instruction <- fetch
    let op = case instruction of
                Inp r     -> inp r
                Add r1 r2 -> add r1 r2
                Mul r1 r2 -> mul r1 r2
                Div r1 r2 -> Main.div r1 r2
                Mod r1 r2 -> Main.mod r1 r2
                Eql r1 r2 -> eql r1 r2
    op

inp :: Register -> State ALU ()
inp r = do
    value <- input
    set r value

operation :: (Int -> Int -> Int) -> Register -> Argument -> State ALU ()
operation op a1 a2 = do
    v1 <- Main.read a1
    v2 <- readArg   a2
    let newValue = v1 `op` v2
    set a1 newValue

add, mul, div, mod, eql :: Register -> Argument -> State ALU ()
add = operation (+)
mul = operation (*)
div = operation Prelude.div
mod = operation Prelude.mod
eql = operation eql'
    where eql' a b = if a == b then 1 else 0

ended :: State ALU Bool
ended = gets $ null . program

fetch :: State ALU Instruction
fetch = state $ \alu ->
    case program alu of
        []   -> error "Cannot fetch instruction"
        x:xs -> (x, alu { program = xs })

input :: State ALU Int
input = state $ \alu ->
    case inputStream alu of
        []   -> error "Input stream is empty"
        x:xs -> (x, alu { inputStream = xs })

readArg :: Argument -> State ALU Int
readArg (R r) = Main.read r
readArg (N v) = gets $ const v

read :: Register -> State ALU Int
read register = gets $ \alu ->
    case register of
        W -> w alu
        X -> x alu
        Z -> z alu
        Y -> y alu

set :: Register -> Int -> State ALU ()
set register newValue = modify $ \alu ->
    case register of
        W -> alu { w = newValue }
        X -> alu { x = newValue }
        Y -> alu { y = newValue }
        Z -> alu { z = newValue }
