module Main where

data Instruction = MoveNext
                 | MovePrev
                 | Increment
                 | Decrement
                 | Print
                 | Store
                 | JumpForward
                 | JumpBack
                 | DebugFront
                 | DebugBack
                 | DebugMemory
                 deriving Show

exampleInputHelloWorld :: String
exampleInputHelloWorld = "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."

exampleIncrementDecrement :: String
exampleIncrementDecrement = "++-.,.@"

debugMax :: Int
debugMax = 10

memory :: [Int]
memory = (repeat 0)

modifyMemory :: [Int] -> Int -> Int -> [Int]
modifyMemory [] _ _ = []
modifyMemory xs i x = modifyMemory' xs 0 i x

modifyMemory' :: [Int] -> Int -> Int -> Int -> [Int]
modifyMemory' (x:xs) current i y = if current == i
                                      then y:xs
                                      else x:(modifyMemory' xs (current+1) i y)

dataPointer :: Int
dataPointer = 0

previousStack :: [Instruction]
previousStack = []

exampleInstructions = map parse exampleIncrementDecrement

main :: IO ()
main = do
  putStrLn "HSBF"
  eval previousStack exampleInstructions memory dataPointer

parse :: Char -> Instruction
parse '>' = MoveNext
parse '<' = MovePrev
parse '+' = Increment
parse '-' = Decrement
parse '.' = Print
parse ',' = Store
parse '[' = JumpForward
parse ']' = JumpBack
parse '@' = DebugFront
parse '#' = DebugBack
parse '$' = DebugMemory

-- eval :: PreviousInstructions -> CurrentInstructions -> Memory -> DataPointer
eval :: [Instruction] -> [Instruction] -> [Int] -> Int -> IO ()
eval [] [] _ _                              = putStrLn "end of evaluation"
eval xs (MoveNext:ys) memory dataPointer    = eval (MoveNext:xs) ys memory (dataPointer+1)
eval xs (MovePrev:ys) memory dataPointer    = eval (MovePrev:xs) ys memory (dataPointer-1)
eval xs (Increment:ys) memory dataPointer   = eval (Increment:xs) ys (modifyMemory memory dataPointer ((memory !! dataPointer)+1)) dataPointer
eval xs (Decrement:ys) memory dataPointer   = eval (Decrement:xs) ys (modifyMemory memory dataPointer ((memory !! dataPointer)-1)) dataPointer
eval xs (Print:ys) memory dataPointer       = do
  -- current data at the memory cell
  putStrLn $ show (memory !! dataPointer)
  -- move to next instruction
  eval (Print:xs) ys memory (dataPointer + 1)
eval xs (Store:ys) memory dataPointer       = do
  char <- getChar
  putStrLn $ show char
  putStrLn $ show (fromEnum char)
  -- move to next instruction
  eval (Store:xs) ys (modifyMemory memory dataPointer (fromEnum char)) (dataPointer+1)
eval xs (JumpForward:ys) memory dataPointer = pure ()
eval xs (JumpBack:ys) memory dataPointer    = pure ()
-- debugging instruction
eval xs (DebugFront:ys) memory dataPointer  = putStrLn (show $ xs)
eval xs (DebugBack:ys) memory dataPointer   = putStrLn (show $ ys)
eval xs (DebugMemory:ys) memory dataPointer = putStrLn (show $ take debugMax $ memory)
