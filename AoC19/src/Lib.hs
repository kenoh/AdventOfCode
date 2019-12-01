module Lib
    ( aocInputFile
    ) where

import Paths_AoC19

aocInputFile :: String -> IO FilePath
aocInputFile = getDataFileName . ("src/data/input-Day" ++)