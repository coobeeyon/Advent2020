module D7 where

import Data.List
import Text.ParserCombinators.Parsec

data Bag  = Bag String String deriving(Show, Eq)
data BagQuantity = BagQuantity Int Bag deriving(Show, Eq)

csvFile = endBy line eol
line = do
  outsideBag <- bag
  whitespace
  string "contain"
  insideBags <- try nobags <|> containedBags
  return (outsideBag, insideBags)

bag = do
  whitespace
  bt <- identifier
  whitespace
  bc <- identifier
  whitespace
  try (string "bags") <|> (string "bag")
  return $ Bag bt bc

containerBag = do
  cb <- bag
  whitespace
  string "contain"
  return cb

containedBags = sepBy bagQuantity (string ", ")

nobags = do
  whitespace
  string "no other bags"
  return []

bagQuantity = do
  whitespace
  numBags <- integer
  whitespace
  bagType <- bag
  return $ BagQuantity (read numBags) bagType

eol = string ".\n"
whitespace = many $ oneOf " \t"
identifier = many1 $ oneOf ['a'..'z']
integer = many1 $ digit

parseCSV :: String -> Either ParseError [(Bag, [BagQuantity])]
parseCSV input = parse csvFile "(unknown)" input

d7Main = do
  input <- readFile "d7.dat"
  case parseCSV input of
    Left e -> do putStrLn "Error parsing input:"
                 print e
    Right r -> mapM_ print r
