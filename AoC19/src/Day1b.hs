module Day1b (run, fuel) where

import Text.Read
import Data.Maybe

import Lib

fuel :: Int -> Int
fuel = sum . (takeWhile (> 0)) . (drop 1) . (iterate fn)
  where fn = (subtract 2) . (flip div 3)

run = do
  input <- aocInputFile "1b" >>= readFile
  let filtered = (mapMaybe (readMaybe :: String -> Maybe Int)) $ lines input
  putStrLn $ show $ sum $ map fuel filtered
  return ()
      