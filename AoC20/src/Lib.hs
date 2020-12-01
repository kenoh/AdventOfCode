module Lib
  ( aocInput
  , intsDwim
  , aocInputInts
  ) where

import Paths_AoC20

aocInput :: String -> IO Text
aocInput fn = do
    pth <- getDataFileName ("src/data/input-D" ++ fn)
    readFileText pth

intsDwim :: Text -> [Integer]
intsDwim input = mapMaybe ((readMaybe :: String -> Maybe Integer) . toString) $ lines input

aocInputInts :: String -> IO [Integer]
aocInputInts fn = do
    input <- aocInput fn
    return $ intsDwim input

