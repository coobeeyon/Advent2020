module D3 where

takeEvery :: Int -> [a] -> [a]
takeEvery n l = map snd $ filter keepRow (zip [0..] l)
  where keepRow (i, _) = i `mod` n == 0

numTrees :: Int -> Int -> [String] -> Int
numTrees dx dy terrain = length $ filter isTree $ ( map head ) mungedTerrain
  where mungedTerrain = zipWith drop [0,dx..] (map cycle (takeEvery dy terrain))
        isTree = (== '#')

d3bSlopes :: [(Int, Int)]
d3bSlopes = [(1, 1),(3, 1),(5, 1),(7, 1),(1, 2)];

d3Data =
  do
    contents <- readFile "d3.dat"
    return (lines contents)

d3Main :: IO()
d3Main = do
  terrain <- d3Data

  let d3aNumTrees = (numTrees 3 1 terrain)
  putStrLn $ "D3a: " ++ (show d3aNumTrees)

  let d3bNumTrees = product [numTrees dx dy terrain | (dx, dy) <- d3bSlopes]
  putStrLn $ "D3b: " ++ (show d3bNumTrees)
