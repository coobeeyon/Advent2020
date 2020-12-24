module D11 where

import Data.Array
import Data.List

data SeatState = O | E | F deriving(Show, Eq)
stateFrom :: Char -> SeatState
stateFrom '.' = F
stateFrom 'L' = E
stateFrom '#' = O

mapElem seatMap row col = seatMap!(row, col)

neighborHood seatMap row col = [  mapElem seatMap r c | x <- [-1..1], y <- [-1..1], (x,y) /= (0,0),
                                  let r = row + y,
                                      let c = col + x,
                                      r > 0, r <= height,
                                      c > 0, c <= width ]
  where (height, width) = snd . bounds $ seatMap

countOccupied :: (Foldable t, Num a) => t SeatState -> a
countOccupied = foldl (flip $ (+) . (\x -> if x == O then 1 else 0)) 0
numOccupied seatMap row col = countOccupied $ neighborHood seatMap row col

newState F _ = F
newState E n | n == 0 = O
             | otherwise = E
newState O n | n > 3 = E
             | otherwise = O

stepMap oldMap = array ((1,1),(rows,cols))  [ ((r,c),newState os nn) | r <- [1..rows], c <- [1..cols], let os = mapElem oldMap r c, let nn = numOccupied oldMap r c]
  where (rows, cols) = snd . bounds $ oldMap

matrixToArray matrix = array ((1,1),(rows, cols)) $ zip indices (concat matrix)
  where rows = length matrix
        cols = length $ head matrix
        indices = [(r,c) | r <- [1..rows], c <- [1..cols]]

loadData fileName = do
  input <- readFile fileName
  return $ matrixToArray $ map (map stateFrom) (lines input)

d11a = do
  seatMap <- loadData "d11.dat"
  let states = iterate stepMap seatMap
      fixedPoint = snd $ last $ takeWhile (uncurry (/=)) $ zip states (tail states)
      finalOccupied = countOccupied fixedPoint
  print finalOccupied
