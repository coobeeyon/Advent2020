module D9 where

import Data.List

loadNums :: String -> IO [Int]
loadNums dataFile = do
  input <- readFile dataFile
  return $  map read (lines input)

pack :: Int -> [Int] -> [(Int, [Int])]
pack prefixLength nums =
  zip (map last chunks) (map (take prefixLength) chunks)
  where chunks = chunk (prefixLength + 1) nums
        chunk cl ns = filter ((>= cl) . length) $ map (take cl) $ tails ns

valid :: (Int, [Int]) -> Bool
valid (num, prefix) = num `elem` (sums prefix)
  where sums nums = [ x + y | x <- nums, y <- nums, not $ x == y ]

mapValid :: Int -> [Int] -> [(Int, Bool)]
mapValid prefixLength nums = zip (drop prefixLength nums) (map valid (pack prefixLength nums))

d9a :: IO Int
d9a = do
  nums <- loadNums "d9.dat"
  return $  (fst . head) ( dropWhile (\(_,v)->v) (mapValid 25 nums) )

d9b :: IO Int
d9b = do
  badNum <- d9a
  nums <- loadNums "d9.dat"
  let subSeq = head $ filter ((==badNum) . sum ) $ concatMap inits (tails nums)
      maxS = maximum subSeq
      minS = minimum subSeq
  return $ maxS + minS
