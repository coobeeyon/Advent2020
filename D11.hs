module D11 where

data SeatState = O | E | F deriving(Show, Eq)
stateFrom :: Char -> SeatState
stateFrom '.' = F
stateFrom 'L' = E
stateFrom '#' = O

loadData :: String -> IO [[SeatState]]
loadData fileName = do
  input <- readFile fileName
  return $ map (map stateFrom) (lines input)
