module Main where

import System.IO
import Control.Monad
import Text.Regex.Base
import Text.Regex.PCRE
import Data.List

d1a :: [Int] -> Int
d1a nums = head [ x * y | x <- nums, y <- nums, x + y == 2020, x <= y ]

d1b :: [Int] -> Int
d1b nums = head [ x * y * z |
                  x <- nums,
                  y <- nums,
                  z <- nums,
                  x + y + z == 2020, x <= y, y <= z ]

d1Main :: IO ()
d1Main = do
  let dataFile = "d1.dat"
  contents <- readFile dataFile
  let nums = [ read x ::Int | x <- lines contents ]
  putStrLn $ "D1a: " ++ show (d1a nums)
  putStrLn $ "D1b: " ++ show (d1b nums)

data PasswordRec = PasswordRec Int Int Char String deriving(Show)

parsePasswordRec :: String -> PasswordRec
parsePasswordRec passwordRec =
  PasswordRec (read num1 ::Int) (read num2 ::Int) (head req) password
  where (_,_,_, [num1, num2, req, password]) =
          (passwordRec =~ "([\\d]*)-([\\d]*) ([a-z]): ([a-z]*)") ::(String, String, String, [String])

policyA :: PasswordRec -> Bool
policyA (PasswordRec minNum maxNum reqChar password) =
  (numFound >= minNum) && (numFound <= maxNum)
  where numFound = length (filter (==reqChar) password)

policyB :: PasswordRec -> Bool
policyB (PasswordRec pos1 pos2 reqChar password) =
  (length (filter (==reqChar) elems)) == 1
  where elems = [password !! (pos1 - 1), password !! (pos2 - 1)]

d2Main :: IO ()
d2Main = do
  let dataFile = "d2.dat"
  contents <- readFile dataFile
  let passwordRecs = map parsePasswordRec (lines contents)
  putStrLn $ "Num satisfying policy a: " ++ ( show . length $ filter policyA passwordRecs )
  putStrLn $ "Num satisfying policy b: " ++ ( show . length $ filter policyB passwordRecs )


main :: IO()
main = do
  d2Main
