module D01a (run, d01a, d01, combs2) where

import Lib
run :: IO ()
run = do
    input <- aocInputInts "01a"
    print $ d01a input


isSum2020 a = (sum a) == 2020

combs2 :: [Integer] -> [[Integer]]
combs2 [_] = []
combs2 (i:is) = (map (\k -> [i, k]) is) ++ combs2 is

d01 :: ([Integer] -> [[Integer]]) -> [Integer] -> Integer
d01 comb ns = product $ fromMaybe [0] $ viaNonEmpty head $ filter isSum2020 $ comb ns

d01a = d01 combs2