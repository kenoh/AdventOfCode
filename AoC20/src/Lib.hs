{-# LANGUAGE OverloadedStrings #-}
module Lib
  ( aocInput
  , intsDwim
  , aocInputInts
  ) where

import Paths_AoC20

aocInput :: FilePath -> IO Text
aocInput fn = do
    pth <- getDataFileName ("src/data/input-D" ++ fn :: FilePath)
    readFile pth

intsDwim :: Text -> [Int]
intsDwim input = mapMaybe (readMaybe . toString) $ lines input

aocInputInts :: FilePath -> IO [Int]
aocInputInts fn = do
    input <- aocInput fn
    return $ intsDwim input