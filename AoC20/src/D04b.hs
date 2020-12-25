{-# LANGUAGE ScopedTypeVariables #-}
module D04b (d04b, run) where


import Data.Map ( lookup )

import Prelude hiding ( (<|>), try )
import Text.Parsec ( parse 
                   , many1
                   , (<|>)
                   , (<?>)
                   , count
                   , eof
                   , choice
                   , parserTraced
                   , try
                   )
import Text.Parsec.Char ( digit 
                        , string 
                        , char
                        , hexDigit
                        )
import Text.Read (read)
import Control.Monad (liftM)
import Universum.Monad.Either (rightToMaybe)

import D04a (d04, PassportValidator, Passport)
import Lib (aocInput)


hgtParser = do
  num <- liftM (read :: String -> Int) $ many1 digit
  unit <- string "cm" <|> string "in"
  eof
  return (num, unit)

vHgt :: String -> Maybe String
vHgt h = do
    (num, unit) <- parse hgtParser "<hgt>" h & rightToMaybe
    case unit of
      "cm" -> guard $ num >= 150 && num <= 193
      "in" -> guard $ num >= 59 && num <= 76
    return h

vHcl :: String -> Maybe ()
vHcl h = parse (char '#' >> count 6 hexDigit >> eof) "<hcl>" h & rightToMaybe

vEcl :: String -> Maybe ()
vEcl e = parse ({- parserTraced "test" -} (choice (map (try . string) opts) >> eof)) "<ecl>" e & rightToMaybe
  where opts = {- traceShowId -} ["amb", "blu", "brn", "grn", "gry", "hzl", "oth"]
  -- TIL: The 'try' above is there because 'string' on itself digests output, which in case
  --      "grn" did not match "gry" would not be tried at all even thought it would have been there. :/
  --      This however is a performance penalty due to backtracking. See (https://stackoverflow.com/questions/33057481/why-does-it-seem-that-the-parsec-choice-operator-depends-on-order-of-the-parsers)

vPid :: String -> Maybe ()
vPid p = parse (count 9 digit >> eof) "<pid>" p & rightToMaybe

vRange :: Passport -> String -> Int -> Int -> Either String Bool
vRange passport name lower upper = do
  x <- (lookup name passport >>= readMaybe :: Maybe Int) & maybeToRight (name ++ " lookup")
  case x >= lower && x <= upper of
    True -> Right True
    False -> Left (name ++ " verification")

keyVerify name passport parser = do
  value <- lookup name passport & maybeToRight (name ++ " lookup failed")
  parser value & maybeToRight (name ++ " parsing failed")

validate :: PassportValidator
validate p = do
  vRange p "byr" 1920 2002
  vRange p "iyr" 2010 2020
  vRange p "eyr" 2020 2030
  keyVerify "hgt" p vHgt
  keyVerify "ecl" p vEcl
  keyVerify "pid" p vPid
  keyVerify "hcl" p vHcl
  return True


d04b :: Text -> Int
d04b = d04 validate

run = do
  input <- aocInput "04"
  putTextLn $ show $ d04b input