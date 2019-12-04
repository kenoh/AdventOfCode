module Main where

import Day1a
import Day1b
import Day2a
import Day2b

import System.Environment

main :: IO ()
main = do
    arg <- getArgs >>= pure . head
    case arg of
        "1a" -> Day1a.run
        "1b" -> Day1b.run
        "2a" -> Day2a.run
        "2b" -> Day2b.run
        _ -> fail "Unknown task"
