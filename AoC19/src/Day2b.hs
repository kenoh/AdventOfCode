module Day2b (run) where

import Lib
import Day2a (intcode, parseProgram)

testRange = [(x, y) | x <- [0..99], y <- [0..99]]

type Prog = [Int]
type In = (Int, Int)
type Out = Int

computation :: Prog -> [(In, Out)]
computation prog = map fn testRange
  where fn input = (input, intcode input prog)

test :: Prog -> In
test = fst . head . dropWhile ((19690720 /=) . snd) . computation

run = do
  input <- aocInputFile "2a" >>= readFile
  let progs = map parseProgram $ lines input
  let counted = map test progs
  putStrLn $ show counted
      
