module Main where

import System.Environment

import qualified D01a
import qualified D01b
import qualified D02a
import qualified D02b
import qualified D03a
import qualified D03b

main :: IO ()
main = do
    arg <- getArgs >>= pure . head'
    case arg of
        "01a" -> D01a.run
        "01b" -> D01b.run
        "02a" -> D02a.run
        "02b" -> D02b.run
        "03a" -> D03a.run
        "03b" -> D03b.run
        _ -> fail "Unknown task. Look into src/ directory for IDs."
