module Main where

import Data.Char

data Instruction = MoveNext
                 | MovePrev
                 | Increment
                 | Decrement
                 | Print
                 | Store
                 | JumpForward
                 | JumpBack
                 | DebugInstructions
                 | DebugMemory
                 deriving Show

exampleInputHelloWorld :: String
exampleInputHelloWorld = 
  "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."

exampleInputHelloWorld2 = "+[-->-[>>+>-----<<]<--<---]>-.>>>+.>>..+++[.>]<<<<.+++.------.<<-.>>>>+."
examplePi = ">+++++++++++++++\
\[<+>>>>>>>>++++++++++<<<<<<<-]>+++++[<+++++++++>-]+>>>>>>+[<<+++[>>[-<]<[>]<-]>>\
\[>+>]<[<]>]>[[->>>>+<<<<]>>>+++>-]<[<<<<]<<<<<<<<+[->>>>>>>>>>>>[<+[->>>>+<<<<]>\
\>>>>]<<<<[>>>>>[<<<<+>>>>-]<<<<<-[<<++++++++++>>-]>>>[<<[<+<<+>>>-]<[>+<-]<++<<+\
\>>>>>>-]<<[-]<<-<[->>+<-[>>>]>[[<+>-]>+>>]<<<<<]>[-]>+<<<-[>>+<<-]<]<<<<+>>>>>>>\
\>[-]>[<<<+>>>-]<<++++++++++<[->>+<-[>>>]>[[<+>-]>+>>]<<<<<]>[-]>+>[<<+<+>>>-]<<<\
\<+<+>>[-[-[-[-[-[-[-[-[-<->[-<+<->>]]]]]]]]]]<[+++++[<<<++++++++<++++++++>>>>-]<\
\<<<+<->>>>[>+<<<+++++++++<->>>-]<<<<<[>>+<<-]+<[->-<]>[>>.<<<<[+.[-]]>>-]>[>>.<<\
\-]>[-]>[-]>>>[>>[<<<<<<<<+>>>>>>>>-]<<-]]>>[-]<<<[-]<<<<<<<<]++++++++++."

exampleLoop = "+++++[-]"

exampleIncrementDecrement :: String
exampleIncrementDecrement = "++.@"

exampleLoopTest :: String
exampleLoopTest = "+++++[+[--.].]@"

examplePlaceholder = ">+++++++++[<++++++++>-]<.>+++++++[<++++>-]<+.+++++++..+++.[-]\
\>++++++++[<++++>-] <.>+++++++++++[<++++++++>-]<-.--------.+++\
\.------.--------.[-]>++++++++[<++++>- ]<+.[-]++++++++++."

-- examplePlaceholder = ">+++++++++[<++++++++>-]<.\
-- \>+++++++[<++++>-]<+.\
-- \+++++++..+++.[-]\
-- \>++++++++[<++++>-] <.\
-- \>+++++++++++[<++++++++>-]<-.--------.+++\
-- \.------.--------.\
-- \[-]>++++++++[<++++>- ]<+.[-]++++++++++."

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
dataPointer = 1

previousStack :: [Instruction]
previousStack = []

-- exampleInstructions = map parse exampleIncrementDecrement
exampleInstructions = map parse $ reverse $ stripSpace exampleInputHelloWorld2
-- exampleInstructions = map parse exampleLoopTest

main :: IO ()
main = do
  putStrLn "HSBF"
  eval exampleInstructions memory 0 0 (repeat 0)

parse :: Char -> Instruction
parse '>' = MoveNext
parse '<' = MovePrev
parse '+' = Increment
parse '-' = Decrement
parse '.' = Print
parse ',' = Store
parse '[' = JumpForward
parse ']' = JumpBack
parse '@' = DebugInstructions
parse '#' = DebugMemory

stripSpace :: String -> String
stripSpace [] = []
stripSpace (x:xs) = if isSpace x
                       then stripSpace xs
                       else (stripSpace xs) ++ [x]

-- eval :: Instructions -> Memory -> InstructionPointer -> DataPointer -> DataPointerStack
eval :: [Instruction] -> [Int] -> Int -> Int -> [Int] -> IO ()
eval instructions memory instructionPointer dataPointer instructionPointerStack = do
  -- putStrLn $ "instructionPointerStack : " ++ (show $ take 5 $ recent:instructionPointerStack)
  if (instructionPointer == (length instructions))
     then putStrLn $ "HSBF END"
     else do
       case (instructions !! instructionPointer) of
         MoveNext -> do
           -- putStrLn "MoveNext"
           eval instructions memory (instructionPointer+1) (dataPointer+1) instructionPointerStack
         MovePrev -> do
           putStrLn $ show $ take 15 memory
           putStrLn "MovePrev"
           eval instructions memory (instructionPointer+1) (dataPointer-1) instructionPointerStack
         Increment -> do
           putStrLn $ "Increment : " ++ (show $ take 15 $ modifyMemory memory dataPointer ((memory !! dataPointer)+1))
           eval instructions (modifyMemory memory dataPointer ((memory !! dataPointer)+1)) (instructionPointer+1) dataPointer instructionPointerStack
         Decrement -> do
           putStrLn $ "Decrement : " ++ (show $ take 15 $ modifyMemory memory dataPointer ((memory !! dataPointer)-1))
           eval instructions (modifyMemory memory dataPointer ((memory !! dataPointer)-1)) (instructionPointer+1) dataPointer instructionPointerStack
         Print -> do
           -- putStrLn $ "Print : " ++ (show (memory !! dataPointer))
           putStrLn $ show $ chr $ (memory !! dataPointer)
           eval instructions memory (instructionPointer+1) dataPointer instructionPointerStack
         Store -> do
           char <- getChar
           -- putStrLn $ "Store : " ++ show (fromEnum char)
           eval instructions memory (instructionPointer+1) dataPointer instructionPointerStack
         JumpForward -> do
           -- putStrLn "JumpForward" 
           if (memory !! dataPointer) == 0
              -- jump to the command after matching ]
              then do
                -- putStrLn $ "jump forward to ]"
                let (recent:restOfStack) = instructionPointerStack
                eval instructions memory recent dataPointer restOfStack
              else do
                -- putStrLn $ "instructionPointer at JumpForward " ++ (show $ instructionPointer+1) ++ " " ++ (show $ instructions !! (instructionPointer+1))
                -- putStrLn $ show $ take 5 $ instructionPointerStack
                eval instructions memory (instructionPointer+1) dataPointer ((instructionPointer+1):instructionPointerStack) -- continue execution
         JumpBack -> do
           putStrLn ("JumpBack " ++ show instructionPointer)
           -- putStrLn $ "dataPointer : " ++ show dataPointer ++ " " ++ show (memory !! dataPointer)
           if (memory !! dataPointer) /= 0
              -- jump back to the command after matching [
              then do 
                -- putStrLn $ "JUMPING BACK" ++ (show $ instructionPointerStack !! 0)
                eval instructions memory (instructionPointerStack !! 0) dataPointer instructionPointerStack
              else do
                -- putStrLn "Continue execution"
                let (recent:restOfStack) = instructionPointerStack
                eval instructions memory (instructionPointer+1) dataPointer restOfStack -- continue execution
         DebugInstructions -> do
           -- putStrLn $ show $ ("DebugInstructions" ++ (show $ instructions))
           eval instructions memory (instructionPointer+1) dataPointer instructionPointerStack
         DebugMemory -> do
           -- putStrLn $ show $ ("DebugMemory" ++ (take 10 $ show $ memory))
           -- putStrLn (show $ memory)
           eval instructions memory (instructionPointer+1) dataPointer instructionPointerStack

