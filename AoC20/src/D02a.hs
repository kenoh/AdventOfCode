{-# LANGUAGE NamedFieldPuns #-}
module D02a (run, d02a, d02ParseInput, d02, run', Line(..)) where

import qualified Text.Parsec as P
import Text.Parsec ( many1
                   , parse
                   )
import Text.Parsec.Char ( char
                        , digit
                        , anyChar
                        )

import Text.Read (read)
import Control.Monad (liftM)

import Lib (aocInput)

run :: IO ()
run = run' d02a

run' fn = do
  input <- d02ParseInput =<< aocInput "02"
  print $ fn input

data Line = Line { from :: Int
                 , to :: Int
                 , subj :: Char
                 , password :: String
                 } deriving (Show)

parseLine :: Text -> Either P.ParseError Line
parseLine = parse lineParser "(input)"

lineParser = do
  from <- liftM (read :: String -> Int) $ many1 digit
  char '-'
  to <- liftM (read :: String -> Int) $ many1 digit
  char ' '
  subj <- anyChar
  char ':'
  char ' '
  password <- many1 anyChar
  return $ Line {from, to, subj, password}


d02ParseInput :: Text -> IO [Line]
d02ParseInput text = do
  let (bads, goods) = partitionEithers $ map parseLine $ lines text
  if bads /= [] then fail "Could not parse some lines."
                else return goods

fits :: Line -> Bool
fits (Line from to subj password) =
  let cnt = length $ filter (subj ==) password in
    (cnt >= from) && (cnt <= to)

d02 :: (Line -> Bool) -> [Line] -> Int
d02 fn = length . filter fn

d02a = d02 fits