module Main where

data Instruction = MoveNext
                 | MovePrev
                 | Increment
                 | Decrement
                 | Print
                 | Store
                 | JumpForward
                 | JumpBack
                 | Debug
                 deriving Show

exampleInputHelloWorld :: String
exampleInputHelloWorld = "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."

exampleIncrementDecrement :: String
exampleIncrementDecrement = "++@"

debugMax :: Int
debugMax = 5

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

exampleInstructions = map parse exampleIncrementDecrement

main :: IO ()
main = do
  putStrLn "HSBF"
  -- eval exampleInstructions memory dataPointer
  eval exampleInstructions memory dataPointer

parse :: Char -> Instruction
parse '>' = MoveNext
parse '<' = MovePrev
parse '+' = Increment
parse '-' = Decrement
parse '.' = Print
parse ',' = Store
parse '[' = JumpForward
parse ']' = JumpBack
parse '@' = Debug

eval :: [Instruction] -> [Int] -> Int -> IO ()
eval [] _ _                              = putStrLn "end of evaluation"
eval (MoveNext:xs) memory dataPointer    = eval xs memory (dataPointer+1)
eval (MovePrev:xs) memory dataPointer    = eval xs memory (dataPointer-1)
eval (Increment:xs) memory dataPointer   = eval xs (modifyMemory memory dataPointer ((memory !! dataPointer)+1)) dataPointer
eval (Decrement:xs) memory dataPointer   = eval xs (modifyMemory memory dataPointer ((memory !! dataPointer)-1)) dataPointer
eval (Print:xs) memory dataPointer       = pure ()
eval (Store:xs) memory dataPointer       = pure ()
eval (JumpForward:xs) memory dataPointer = pure ()
eval (JumpBack:xs) memory dataPointer    = pure ()
eval (Debug:xs) memory dataPointer       = putStrLn (show $ take debugMax $ memory)
