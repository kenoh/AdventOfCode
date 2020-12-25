{-# LANGUAGE OverloadedStrings #-}
module D04a (d04a, run, d04, PassportValidator, validPassports, Passport) where

import Prelude hiding ((<|>), try)
import Text.Parsec
    ( stateInput,
      getParserState,
      ParsecT,
      eof,
      lookAhead,
      many1,
      manyTill,
      notFollowedBy,
      parse,
      parserTrace,
      parserTraced,
      sepBy,
      skipMany1,
      try,
      (<?>),
      (<|>),
      sepEndBy,
      satisfy )
import Text.Parsec.Char (endOfLine, letter, char, string, spaces)
import Data.Map (fromList, member)
import Data.Char ( isSpace )

import Lib (aocInput)
type Passport = Map String String


-- FIXME: This whole thing does not work if there is a blank line at the EOF. :(


-- | Here we match the key-value pair. The `<?>` is for better orientation.
entry = do
  -- parserTrace "wordStart"
  key <- many1 letter <?> "key"
  char ':'
  value <- many1 (satisfy (not . isSpace)) <?> "value"
  -- parserTrace "wordEnd"
  return (key, value)


-- | 'try' here is to not digest the stuff since it might be needed to match the 'passports deleimiter' in
-- `fileParser'
entryDelimiter = try ((endOfLine >> notFollowedBy endOfLine) <|> skipMany1 (char ' ')) <?> "entry delimiter"

passport = sepBy entry entryDelimiter <?> "passport"

fileParser = do
  contents <- sepBy passport (string "\n\n") <?> "file"
  eof
  return contents


type PassportValidator = Passport -> Either String Bool

mustFields :: PassportValidator
mustFields entries = all (`member` entries) fields & orFailWith "mustFields failed"
  where fields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

passports :: Text -> [Passport]
passports input = case parse fileParser "(blah)" input of
  Left e -> error (show e) []
  Right ps -> map fromList ps

validPassports :: PassportValidator -> Text -> [Passport]
validPassports validator = filterEither ({- debugFilter -} validator) . passports

-- magic ((.) . (.)) is . which takes two args, not just one. :)
d04 :: PassportValidator -> Text -> Int
d04 = ((.) . (.)) length validPassports

d04a :: Text -> Int
d04a = d04 mustFields

run = do
  input <- aocInput "04"
  putTextLn $ show $ d04a input