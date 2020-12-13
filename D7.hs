module D7 where

import Data.List
import Text.ParserCombinators.Parsec

data Bag  = Bag String String deriving(Show, Ord,Eq)
data BagQuantity = BagQuantity Int Bag deriving(Show)
type ContainMap = [(Bag, [BagQuantity])]

--
-- Parser
--
fileP = endBy lineP eol
lineP = do
  outsideBag <- bagP
  whitespaceP
  string "contain"
  insideBags <- try noBagsP <|> containedBagsP
  return (outsideBag, insideBags)

bagP = do
  bt <- identifierP
  bc <- identifierP
  whitespaceP
  try (string "bags") <|> (string "bag")
  return $ Bag bt bc

containerBagP= do
  cb <- bagP
  whitespaceP
  string "contain"
  return cb

containedBagsP = sepBy bagQuantityP (string ", ")

noBagsP = do
  whitespaceP
  string "no other bags"
  return []

bagQuantityP = do
  numBags <- integerP
  bagType <- bagP
  return $ BagQuantity (read numBags) bagType

eol = string ".\n"
whitespaceP = many $ oneOf " \t"
identifierP = whitespaceP >> (many1 $ oneOf ['a'..'z'])
integerP = whitespaceP >> (many1 $ digit)

parseFile :: String -> Either ParseError ContainMap
parseFile input = parse fileP "(unknown)" input

--
-- Solution
--
mergeSorted :: (Eq a, Ord a) => [a] -> [a] ->[a]
mergeSorted x [] = x
mergeSorted [] x = x
mergeSorted (x:xs) (y:ys)
  | (x == y) = x : mergeSorted xs ys
  | (x <  y) = x : mergeSorted xs (y:ys)
  | otherwise  = y : mergeSorted (x:xs) ys

mergeMap :: Ord b => (a -> [b]) -> [a] -> [b]
mergeMap _ [] = []
mergeMap f (x:xs) = mergeSorted (f x) (mergeMap f xs)

directContainBag :: ContainMap -> Bag -> [Bag]
directContainBag cmap bag =
  [ holder | (holder, held) <- cmap, bag `elem ` (map bagType held)]
  where bagType (BagQuantity _ t) = t

canContainBag :: ContainMap -> Bag -> [Bag]
canContainBag cmap bag =
  if (canContainThis == []) then
    []
  else
    mergeSorted canContainThis (mergeMap (canContainBag cmap) canContainThis)
  where canContainThis = directContainBag cmap bag

d7Data :: String -> IO ContainMap
d7Data fileName = do
  input <- readFile fileName
  case parseFile input of
    Left e -> do putStrLn "Error parsing input:"
                 print e
                 return []
    Right r -> return $ sortOn fst (map sortContainedBags r)
               where sortContainedBags (b, bq) = (b, sortOn bagType bq)
                     bagType (BagQuantity _ t) = t

numContained :: ContainMap -> BagQuantity -> Int
numContained cmap (BagQuantity n b) =
  n * ( 1 + case contained of
              Just bags -> sum ( map ( numContained cmap ) bags )
              Nothing -> 0
      )
  where contained = lookup b cmap

d7Main :: IO ()
d7Main = do
  bagContentsMap <- d7Data "d7.dat"
  let shinyGoldContainers = canContainBag bagContentsMap (Bag "shiny" "gold")
  mapM_ print shinyGoldContainers
  print $ length shinyGoldContainers
  let shinyGoldContained = numContained bagContentsMap (BagQuantity 1 (Bag "shiny" "gold")) - 1
  print shinyGoldContained
