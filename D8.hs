module D8 where

import Data.List
import Text.ParserCombinators.Parsec

--
-- Machine Definitions
--
data Instruction = NOP Int
            | ACC Int
            | JMP Int
            | UNK
            deriving(Show)

type Program = [Instruction]

data MachineState  = MachineState {acc :: Int, sp :: Int}
                   | Undef
                   deriving(Show, Eq)

isUndef :: MachineState -> Bool
isUndef Undef = True
isUndef _ = False

bootState :: MachineState
bootState = MachineState{acc=0, sp=0}

exec :: Instruction -> MachineState -> MachineState
exec UNK     _       = Undef
exec _         Undef = Undef
exec (NOP _) inState = inState{ sp  = (sp inState)  + 1}
exec (ACC n) inState = inState{ acc = (acc inState) + n,
                                sp  = (sp  inState) + 1}
exec (JMP n) inState = inState{ sp  = (sp inState)  + n}

step :: Program -> MachineState -> MachineState
step _ Undef = Undef
step p s
  | stackIndex < (length p) = exec opCode s
  | otherwise = Undef
  where
    stackIndex = sp s
    opCode = p !! stackIndex

--
-- Parser
--
deCode :: String -> Int -> Instruction
deCode "acc" n = ACC n
deCode "nop" n = NOP n
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

loadProgram :: String -> IO Program
loadProgram fileName = do
  input <- readFile fileName
  case parseFile input of
    Left e -> do putStrLn "Error parsing input:"
                 print e
                 return []
    Right r -> return r

--
-- Solution
--
toggle :: Instruction -> Instruction
toggle (NOP n) = JMP n
toggle (JMP n) = NOP n
toggle x = x

mutate :: Program -> Int -> Program
mutate p i = h ++ [(toggle . head) t] ++ (tail t)
  where (h,t) = splitAt i p

allMutations :: Program -> [Program]
allMutations p = map (mutate p) [0..(l - 1)]
  where l = length p

collisionPoint :: (MachineState->MachineState) -> MachineState -> (MachineState, MachineState)
collisionPoint f x = race x (f x)
  where
    race slow Undef = (slow, Undef)
    race slow fast | ((sp slow) == (sp fast)) = (slow, fast)
                   | otherwise = race (f slow) ((f.f) fast)

convergencePoint :: (MachineState->MachineState) -> MachineState -> MachineState -> MachineState
convergencePoint f x y
  | ((sp x) == (sp y)) = x
  | otherwise = convergencePoint f (f x) (f y)

cycleStart :: (MachineState->MachineState) -> MachineState -> MachineState
cycleStart f x = convergencePoint f x (f cp)
  where (cp, _) = collisionPoint f x

d8a :: IO ()
d8a = do
  program <- loadProgram "d8.dat"
  let cs = cycleStart (step program) bootState
      runCycle = iterate (step program) cs
      endCycle = last $ takeWhile ((not . (== (sp cs))) . sp)(tail runCycle)
  print $ endCycle

d8b :: IO ()
d8b = do
  program <- loadProgram "d8.dat"
  let cPoints = map programCollision (allMutations program)
        where programCollision p = snd $ collisionPoint (step p) bootState
  let terminating = [(x,y) | (x,y) <- zip [0..] cPoints, isUndef y]
      mutIndex = fst . head $ terminating
      mutProgram = mutate program mutIndex
      progRun = iterate (step mutProgram) bootState
  print $ last $ takeWhile (not . isUndef) progRun
