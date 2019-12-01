module Day1a (run) where

import Text.Read
import Data.Maybe

import Lib

run = do
  input <- aocInputFile "1a" >>= readFile
  let filtered = (mapMaybe (readMaybe :: String -> Maybe Integer)) $ lines input
  let counted = map ((subtract 2) . (flip div 3)) filtered
  -- putStrLn $ show filtered
  -- putStrLn $ show counted
  putStrLn $ show $ sum counted
      