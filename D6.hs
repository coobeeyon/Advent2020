module D6 where

import Data.List
import Data.List.Split

d6Data =
  do
    contents <- readFile "d6.dat"
    let contentLines = lines contents
        recordLines = splitOn [""] contentLines
    return recordLines

numAllAnswered answerGroup =
  length [head l | l <- sortedAnswers, length l == groupLength]
  where sortedAnswers = (group . sort . concat) answerGroup
        groupLength = length answerGroup

d6Main =
  do
    groups <- d6Data
    let scoresA = map (length . group . sort . concat) groups
    putStrLn $ "D6a Sum of scores: " ++ show (sum scoresA)
    let scoresB = map numAllAnswered groups
    putStrLn $ "D6b Sum of scores: " ++ show ( sum scoresB )
