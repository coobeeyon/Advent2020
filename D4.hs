module D4 where

import Data.List.Split
import Data.Char

getKeys = map head

validNumDigits :: Int -> String -> Bool
validNumDigits numDigits numString =
  length numString == numDigits &&
  all isDigit numString

validNum :: Int -> Int -> Int -> String -> Bool
validNum numDigits minNum maxNum numString =
  validNumDigits numDigits numString &&
  inRange (read numString)
  where inRange num = (num >= minNum) && (num <= maxNum)

validYear = validNum 4
validHeightCm = validNum 3 150 193
validHeightIn = validNum 2 59 76

validHairColor :: String -> Bool
validHairColor hairColor =
  length hairColor == 7 &&
  head hairColor == '#' &&
  all (`elem` hexDigits) (drop 1 hairColor)
  where hexDigits = ['a'..'f']++['0'..'9']

validEyeColor eyeColor =
  eyeColor `elem` [ "amb", "blu", "brn", "gry", "grn", "hzl", "oth" ]

validEntry :: String -> String -> Bool
validEntry "byr" = validYear 1920 2002
validEntry "iyr" = validYear 2010 2020
validEntry "eyr" = validYear 2020 2030
validEntry "hgt" = \value ->
    let units = dropWhile isDigit value in
      case units of
        "in" -> validHeightIn (take 2 value)
        "cm" -> validHeightCm (take 3 value)
        _ -> False
validEntry "hcl" = validHairColor
validEntry "ecl" = validEyeColor
validEntry "pid" = validNumDigits 9
validEntry "cid" = \_->True
validEntry _ = \_->False

validRecordA record = keyLength == 8 ||
                      keyLength == 7 && not (elem "cid" keys)
  where keys = getKeys record
        keyLength = length keys

validRecordB record = validRecordA record &&
                      all (uncurry validEntry) [(key, val) | [key,val] <- record]

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
  let numValidA = length $ filter validRecordA splitRecords
  putStrLn $ "D4a Num valid: " ++ (show numValidA)
  let numValidB = length $ filter validRecordB splitRecords
  putStrLn $ "D4b Num valid: " ++ (show numValidB)
