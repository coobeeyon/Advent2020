module D7 where

import Data.List
import Text.ParserCombinators.Parsec

data Bag  = Bag String String deriving(Show, Eq)
data BagQuantity = BagQuantity Int Bag deriving(Show, Eq)

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

type ContainMap = [(Bag, [BagQuantity])]

parseFile :: String -> Either ParseError ContainMap
parseFile input = parse fileP "(unknown)" input

directContainBag :: ContainMap -> Bag -> [Bag]
directContainBag cmap bag =
  [ holder | (holder, held) <- cmap, bag `elem ` (map bagType held)]
  where bagType (BagQuantity _ t) = t

canContainBag :: ContainMap -> Bag -> [Bag]
canContainBag cmap bag =
  if (canContainThis == []) then
    []
  else
    canContainThis ++ (nub (concatMap (canContainBag cmap) canContainThis))
  where canContainThis = directContainBag cmap bag

d7Data :: String -> IO ContainMap
d7Data fileName = do
  input <- readFile fileName
  case parseFile input of
    Left e -> do putStrLn "Error parsing input:"
                 print e
                 return []
    Right r -> return r

d7Main = do
  bagContentsMap <- d7Data "d7test.dat"
  mapM_ print (canContainBag bagContentsMap (Bag "shiny" "gold"))
