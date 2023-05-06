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
                 | Unknown
                 deriving Show

exampleInputHelloWorld = 
  "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."

exampleInputHelloWorld3 = ">++++++++[-<+++++++++>]<.>>+>-[+]++>++>+++[>[->+++<<+++>]<<]>-----.>->\
\+++..+++.>-.<<+[>[+>+]>>]<--------------.>>.+++.------.--------.>+.>+."

exampleInputHelloWorld4 = "++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>."

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

exampleCollatz = ">,[\
\    [\
\        ----------[\
\            >>>[>>>>]+[[-]+<[->>>>++>>>>+[>>>>]++[->+<<<<<]]<<<]\
\            ++++++[>------<-]>--[>>[->>>>]+>+[<<<<]>-],<\
\        ]>\
\    ]>>>++>+>>[\
\        <<[>>>>[-]+++++++++<[>-<-]+++++++++>[-[<->-]+[<<<<]]<[>+<-]>]\
\        >[>[>>>>]+[[-]<[+[->>>>]>+<]>[<+>[<<<<]]+<<<<]>>>[->>>>]+>+[<<<<]]\
\        >[[>+>>[<<<<+>>>>-]>]<<<<[-]>[-<<<<]]>>>>>>>\
\    ]>>+[[-]++++++>>>>]<<<<[[<++++++++>-]<.[-]<[-]<[-]<]<,\
\]"

exampleGoldenRatio = "\
\+>>>>>>>++>+>+>+>++<[\
\    +[\
\        --[++>>--]->--[\
\            +[\
\                +<+[-<<+]++<<[-[->-[>>-]++<[<<]++<<-]+<<]>>>>-<<<<\
\                <++<-<<++++++[<++++++++>-]<.---<[->.[-]+++++>]>[[-]>>]\
\            ]+>>--\
\        ]+<+[-<+<+]++>>\
\    ]<<<<[[<<]>>[-[+++<<-]+>>-]++[<<]<<<<<+>]\
\    >[->>[[>>>[>>]+[-[->>+>>>>-[-[+++<<[-]]+>>-]++[<<]]+<<]<-]<]]>>>>>>>\
\]"


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

exampleInstructions = map parse $ reverse $ stripSpace exampleCollatz

main :: IO ()
main = do
  putStrLn "HSBF"
  eval exampleInstructions memory 0 10 (repeat 0) False 0

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
parse _   = Unknown

stripSpace :: String -> String
stripSpace [] = []
stripSpace (x:xs) = if isSpace x
                       then stripSpace xs
                       else (stripSpace xs) ++ [x]

-- eval :: Instructions -> Memory -> InstructionPointer -> DataPointer -> DataPointerStack -> IsJumping -> BracketCounter
eval :: [Instruction] -> [Int] -> Int -> Int -> [Int] -> Bool -> Int -> IO ()
eval instructions memory instructionPointer dataPointer instructionPointerStack True bracketCounter = do
  -- putStrLn $ "got into this case " ++ show instructionPointer
  case (instructions !! instructionPointer) of 
    JumpForward -> do
      putStrLn "IsJumping [JumpForward]"
      eval instructions memory (instructionPointer+1) dataPointer instructionPointerStack True (bracketCounter+1)
    JumpBack -> if (bracketCounter-1) == 0
                   then do 
                     putStrLn "IsJumping [JumpBack] EXIT"
                     eval instructions memory (instructionPointer+1) dataPointer instructionPointerStack False 0
                   else do
                     putStrLn $ "IsJumping [JumpBack] JUMPING " ++ show (bracketCounter-1)
                     eval instructions memory (instructionPointer+1) dataPointer instructionPointerStack True (bracketCounter-1)
    _ -> do
      putStrLn $ "IsJumping [ANY] " ++ show (instructions !! instructionPointer)
      eval instructions memory (instructionPointer+1) dataPointer instructionPointerStack True bracketCounter
eval instructions memory instructionPointer dataPointer instructionPointerStack False bracketCounter = do
  if (instructionPointer == (length instructions))
     then do
       putStrLn $ "instruction pointer : " ++ show instructionPointer
       putStrLn $ "(length instructions) : " ++ show (length instructions)
       putStrLn $ "HSBF END"
     else do
       case (instructions !! instructionPointer) of
         MoveNext -> do
           putStrLn "MoveNext"
           eval instructions memory (instructionPointer+1) (dataPointer+1) instructionPointerStack False bracketCounter
         MovePrev -> do
           -- putStrLn $ show $ take 15 memory
           putStrLn "MovePrev"
           eval instructions memory (instructionPointer+1) (dataPointer-1) instructionPointerStack False bracketCounter
         Increment -> do
           putStrLn $ "Increment : " ++ (show $ take 15 $ modifyMemory memory dataPointer ((memory !! dataPointer)+1))
           eval instructions (modifyMemory memory dataPointer ((memory !! dataPointer)+1)) 
             (instructionPointer+1) dataPointer instructionPointerStack False bracketCounter
         Decrement -> do
           putStrLn $ "Decrement : " ++ (show $ take 15 $ modifyMemory memory dataPointer ((memory !! dataPointer)-1))
           eval instructions (modifyMemory memory dataPointer ((memory !! dataPointer)-1))
             (instructionPointer+1) dataPointer instructionPointerStack False bracketCounter
         Print -> do
           -- putStrLn $ "Print : " ++ (show (memory !! dataPointer))
           putStrLn $ "Print : " ++ (show $ chr $ (memory !! dataPointer))
           eval instructions memory (instructionPointer+1) dataPointer instructionPointerStack False bracketCounter
         Store -> do
           char <- getChar
           putStrLn $ "Store : " ++ show (fromEnum char)
           eval instructions (modifyMemory memory dataPointer (fromEnum char)) (instructionPointer+1) dataPointer instructionPointerStack False bracketCounter
           -- eval instructions memory (instructionPointer+1) dataPointer instructionPointerStack False bracketCounter
         JumpForward -> do
           if (memory !! dataPointer) == 0
              -- jump to the command after matching ]
              then do
                putStrLn $ "JumpForward to ]"
                -- let (recent:restOfStack) = instructionPointerStack
                eval instructions memory (instructionPointer+1) dataPointer instructionPointerStack True (bracketCounter+1)
              else do
                -- continue execution
                -- putStrLn $ show $ take 5 $ instructionPointerStack
                putStrLn $ "JumpForward continue executing"
                eval instructions memory (instructionPointer+1) dataPointer ((instructionPointer+1):instructionPointerStack) False bracketCounter
         JumpBack -> do
           putStrLn ("JumpBack " ++ show instructionPointer)
           -- putStrLn $ "dataPointer : " ++ show dataPointer ++ " " ++ show (memory !! dataPointer)
           if (memory !! dataPointer) /= 0
              -- jump back to the command after matching [
              then do 
                -- putStrLn $ "JUMPING BACK" ++ (show $ instructionPointerStack !! 0)
                putStrLn "JumpBack to ["
                eval instructions memory (instructionPointerStack !! 0) dataPointer instructionPointerStack False bracketCounter
              else do
                -- continue execution
                putStrLn "JumpBack continue executing"
                let (recent:restOfStack) = instructionPointerStack
                eval instructions memory (instructionPointer+1) dataPointer restOfStack False bracketCounter
         Unknown -> eval instructions memory (instructionPointer+1) dataPointer instructionPointerStack False bracketCounter
         DebugInstructions -> do
           -- putStrLn $ show $ ("DebugInstructions" ++ (show $ instructions))
           eval instructions memory (instructionPointer+1) dataPointer instructionPointerStack False bracketCounter
         DebugMemory -> do
           -- putStrLn $ show $ ("DebugMemory" ++ (take 10 $ show $ memory))
           -- putStrLn (show $ memory)
           eval instructions memory (instructionPointer+1) dataPointer instructionPointerStack False bracketCounter

