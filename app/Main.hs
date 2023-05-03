module Main where

data Instruction = MoveNext
                 | MovePrev
                 | Increment
                 | Decrement
                 | Print
                 | Store
                 | JumpForward
                 | JumpBack
                 deriving Show

exampleInputHelloWorld :: String
exampleInputHelloWorld = "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."

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

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"

parse :: Char -> Instruction
parse '>' = MoveNext
parse '<' = MovePrev
parse '+' = Increment
parse '-' = Decrement
parse '.' = Print
parse ',' = Store
parse '[' = JumpForward
parse ']' = JumpBack

exampleInstructions = map parse exampleInputHelloWorld

eval :: [Instruction] -> Bool
eval []               = True
eval (MoveNext:xs)    = False
eval (MovePrev:xs)    = False
eval (Increment:xs)   = False
eval (Decrement:xs)   = False
eval (Print:xs)       = False
eval (Store:xs)       = False
eval (JumpForward:xs) = False
eval (JumpBack:xs)    = False
