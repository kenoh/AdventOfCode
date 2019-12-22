module Main where

import Day1a
import Day1b
import Day2a
import Day2b
import Day3a
import Day3b

import System.Environment

main :: IO ()
main = do
    arg <- getArgs >>= pure . head
    case arg of
        "1a" -> Day1a.run
        "1b" -> Day1b.run
        "2a" -> Day2a.run
        "2b" -> Day2b.run
        "3a" -> Day3a.run
        "3b" -> Day3b.run
        _ -> fail "Unknown task"
