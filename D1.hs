module D1 where

d1a :: [Int] -> Int
d1a nums = head [ x * y | x <- nums, y <- nums, x + y == 2020, x <= y ]

d1b :: [Int] -> Int
d1b nums = head [ x * y * z |
                  x <- nums,
                  y <- nums,
                  z <- nums,
                  x + y + z == 2020, x <= y, y <= z ]

d1Main :: IO ()
d1Main = do
  let dataFile = "d1.dat"
  contents <- readFile dataFile
  let nums = [ read x | x <- lines contents ]
  putStrLn $ "D1a: " ++ show (d1a nums)
  putStrLn $ "D1b: " ++ show (d1b nums)
