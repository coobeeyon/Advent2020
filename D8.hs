module D8 where

import Text.ParserCombinators.Parsec

--
-- Machine Definitions
--
data Instruction = NOP
            | ACC Int
            | JMP Int
            | UNK
            deriving(Show)

type Program = [Instruction]

data MachineState  = MachineState {acc :: Int, sp :: Int}
                   | Undef
                   deriving(Show, Eq)

bootState :: MachineState
bootState = MachineState{acc=0, sp=0}

exec :: Instruction -> MachineState -> MachineState
exec UNK     _       = Undef
exec _         Undef = Undef
exec NOP     inState = inState{ sp  = (sp inState)  + 1}
exec (ACC n) inState = inState{ acc = (acc inState) + n,
                                sp  = (sp  inState) + 1}
exec (JMP n) inState = inState{ sp  = (sp inState)  + n}

step :: Program -> MachineState -> MachineState
step _ Undef = Undef
step p s = exec opCode s
  where opCode = p !! (sp s)

--
-- Parser
--
deCode :: String -> Int -> Instruction
deCode "acc" n = ACC n
deCode "nop" n = NOP
deCode "jmp" n = JMP n
deCode _     _ = UNK

fileP = endBy lineP eol
lineP = do
  opCode <- identifierP
  argument <- integerP
  return $ deCode opCode (read argument)

eol = string "\n"
whitespaceP = many $ oneOf " \t"
identifierP = whitespaceP >> (many1 $ oneOf ['a'..'z'])
integerP = do
  whitespaceP
  optional (char '+')
  many1 $ (oneOf (['0'..'9']++['-']))

parseFile :: String -> Either ParseError Program
parseFile input = parse fileP "Program" input

--
-- Solution
--
collisionPoint :: Eq b => (a->b) -> (a->a) -> a -> a
collisionPoint pr f x = race x (f x)
  where race slow fast | ((pr slow) == (pr fast)) = slow
                       | otherwise = race (f slow) ((f.f) fast)

convergencePoint :: Eq b => (a->b) -> (a->a) -> a -> a -> a
convergencePoint pr f x y
  | ((pr x) == (pr y)) = x
  | otherwise = convergencePoint pr f (f x) (f y)

cycleStart :: Eq b => (a->b) -> (a->a) -> a -> a
cycleStart pr f x = convergencePoint pr f x (f cp)
  where cp = collisionPoint pr f x

loadProgram :: String -> IO Program
loadProgram fileName = do
  input <- readFile fileName
  case parseFile input of
    Left e -> do putStrLn "Error parsing input:"
                 print e
                 return []
    Right r -> return r

d7a :: IO ()
d7a = do
  program <- loadProgram "d8.dat"
  let cs = cycleStart sp (step program) bootState
      runCycle = iterate (step program) cs
      endCycle = last $ takeWhile ((not . (== (sp cs))) . sp)(tail runCycle)
  print $ endCycle
