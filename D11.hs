module D11 where

data SeatState = O | E | F deriving(Show, Eq)
stateFrom :: Char -> SeatState
stateFrom '.' = F
stateFrom 'L' = E
stateFrom '#' = O
type SeatMap = [[SeatState]]

dims :: SeatMap -> (Int, Int)
dims seatMap = (length seatMap, length . head $ seatMap)

mapElem seatMap row col = (seatMap !! row) !! col

neighborHood seatMap row col = [  mapElem seatMap r c | x <- [-1..1], y <- [-1..1], (x,y) /= (0,0),
                                  let r = row + y,
                                      let c = col + x,
                                      r >= 0, r < height,
                                      c >= 0, c < width ]
  where (height, width) = dims seatMap

countOccupied = sum . map (\x -> if x == O then 1 else 0)
numOccupied seatMap row col = countOccupied $ neighborHood seatMap row col

newState F _ = F
newState E n | n == 0 = O
             | otherwise = E
newState O n | n > 3 = E
             | otherwise = O

stepMap oldMap =[ [ newState os nn | c <- [0..(width - 1)], let os = mapElem oldMap r c, let nn = numOccupied oldMap r c] | r <-[0..(height - 1)]]
  where (height, width) = dims oldMap

loadData :: String -> IO SeatMap
loadData fileName = do
  input <- readFile fileName
  return $ map (map stateFrom) (lines input)

d11a = do
  seatMap <- loadData "d11.dat"
  let states = iterate stepMap seatMap
      fixedPoint = snd $ last $ takeWhile (uncurry (/=)) $ zip states (tail states)
      finalOccupied = sum $ map countOccupied fixedPoint
  print finalOccupied
