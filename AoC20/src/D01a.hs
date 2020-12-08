module D01a (run, d01a, d01, combs2) where

import Lib
run :: IO ()
run = do
    input <- aocInputInts "01a"
    putTextLn $ show $ d01a input


isSum2020 :: [Int] -> Bool
isSum2020 a = (sum a) == 2020

combs2 :: [Int] -> [[Int]]
combs2 [_] = []
combs2 (i:is) = (map (\k -> [i, k]) is) ++ combs2 is

d01 :: ([Int] -> [[Int]]) -> [Int] -> Int
d01 comb ns = product $ head' $ filter isSum2020 $ comb ns

d01a = d01 combs2