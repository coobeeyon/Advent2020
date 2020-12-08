module D5 where

import Data.List

powers :: Int -> [Int]
powers numPowers =
  reverse $ take numPowers $ iterate (2*) 1

codeNum :: Char -> Int
codeNum 'B' = 1
codeNum 'R' = 1
codeNum _   = 0

decodeNum :: String -> Int
decodeNum code = sum $ zipWith (*) (powers (length code)) (map codeNum code)

decode :: String -> (Int, Int)
decode code = (decodeNum row, decodeNum seat)
  where (row, seat) = splitAt 7 code

seatId :: (Int, Int) -> Int
seatId (row, seat) = 8 * row + seat

findMySeat openSeats =
  [seatId mid | (left, mid, right) <- filter midSeat (zip3 openSeats (drop 1 openSeats) (drop 2 openSeats))]
  where midSeat (left, mid, right) = rightId - midId > 1 &&  midId - leftId > 1
          where leftId  = seatId left
                midId   = seatId mid
                rightId = seatId right

d5Data =
  do
    contents <- readFile "d5.dat"
    return $ map decode (lines contents)

d5Main = do
  seatMap <- d5Data
  let highestID = maximum $ map seatId seatMap
  putStrLn $ "D5a Highest ID: " ++ show highestID
  let openSeats = sortOn seatId (filter (not . (`elem` seatMap)) allSeats)
        where allSeats = [ (row, seat) | row <- [1..126], seat <- [0..7] ]
  putStrLn $ "D5b open seats: " ++ show ( findMySeat openSeats )
