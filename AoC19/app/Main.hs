module Main where

import Day1a
import Day1b

import System.Environment

main :: IO ()
main = do
    arg <- getArgs >>= pure . head
    case arg of
        "1a" -> Day1a.run
        "1b" -> Day1b.run
        _ -> fail "Unknown task"