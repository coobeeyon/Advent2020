module D10 where

import Data.List

loadNums :: String -> IO [Integer]
loadNums dataFile = do
  input <- readFile dataFile
  return $  map read (lines input)

d10a = do
  nums <- loadNums "d10.dat"
  let adaptors = sort ( 0 : (maximum nums + 3) : nums)
      steps = zipWith (-) (tail adaptors) adaptors
      stepGroups = (group . sort) steps
  return $ groupCounts stepGroups
    where groupCounts (x:xs) = (head x, length x) : groupCounts xs
          groupCounts [] = []

numChains :: Integer -> Integer
numChains chainLength = iter chainLength (1,0,0)
  where iter cl (n1, n2, n3) | cl == 1 = n1 + n2 + n3
                             | otherwise = iter (cl - 1) (n1 + n2 + n3, n1, n2)

d10b = do
  nums <- loadNums "d10.dat"
  let adaptors = sort ( 0 : (maximum nums + 3) : nums)
      steps = zipWith (-) (tail adaptors) adaptors
      stepGroups = group steps
      numSubChains = map (\(x:xs)->if (x==1) then numChains (1 + (genericLength xs)) else 1) stepGroups
  return $ product numSubChains
