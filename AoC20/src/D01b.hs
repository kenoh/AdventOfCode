module D01b (run, d01b) where

import Lib
import D01a (combs2, d01)

run :: IO ()
run = do
    input <- aocInputInts "01"
    putTextLn $ show $ d01b input


isSum2020 (a, b) = (a + b) == 2020

combs3 :: [Int] -> [[Int]]
combs3 [_, _] = []
combs3 (i:is) = (map (\k -> [i] ++ k) $ combs2 is) ++ combs3 is

d01b :: [Int] -> Int
d01b = d01 combs3

