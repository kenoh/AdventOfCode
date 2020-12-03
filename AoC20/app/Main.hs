module Main where

import System.Environment

import qualified D01a
import qualified D01b
import qualified D02a
import qualified D02b

main :: IO ()
main = do
    arg <- getArgs >>= pure . viaNonEmpty head
    case arg of
        Just "01a" -> D01a.run
        Just "01b" -> D01b.run
        Just "02a" -> D02a.run
        Just "02b" -> D02b.run
        Nothing -> fail "Missing task."
        _ -> fail "Unknown task. Look into src/ directory for IDs."
