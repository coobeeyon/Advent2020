module D4 where

import Data.List.Split

getKeys = map head

valid record = (keyLength == 8) || ((keyLength == 7) && (not (elem "cid" keys)))
  where keys = getKeys record
        keyLength = length keys

d4Data =
  do
    contents <- readFile "d4.dat"
    let contentLines = lines contents
        recordLines = splitOn [""] contentLines
        recordWords = map (map words) recordLines
        recordLists = map concat recordWords
        splitRecords = map ( map ( splitOn ":" ) ) recordLists
    return splitRecords

d4Main :: IO ()
d4Main = do
  splitRecords <- d4Data
  let numValid = length $ filter valid splitRecords
  putStrLn $ "D4a Num valid: " ++ (show numValid)
