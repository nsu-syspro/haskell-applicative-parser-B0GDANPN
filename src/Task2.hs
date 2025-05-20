{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

module Task2 where

import Parser
import ParserCombinators
import Control.Applicative ( (<|>), some )
import Data.Char (digitToInt)
-- | Date representation
--
-- Date parts are expected to be in following ranges
--
-- 'Day' in @[1..31]@
-- 'Month' in @[1..12]@
-- 'Year' is any non-negative integer
--
data Date = Date Day Month Year
  deriving (Show, Eq)

newtype Day   = Day   Int deriving (Show, Eq)
newtype Month = Month Int deriving (Show, Eq)
newtype Year  = Year  Int deriving (Show, Eq)

-- | Parses date in one of three formats given as BNF
--
-- @
-- date ::= dotFormat | hyphenFormat | usFormat
--
-- dotFormat ::= day "." month "." year
-- hyphenFormat ::= day "-" month "-" year
-- usFormat ::= monthName " " usDay " " year
--
-- usDay ::= nonZeroDigit | "1" digit | "2" digit | "30" | "31"
-- day ::= "0" nonZeroDigit | "1" digit | "2" digit | "30" | "31"
-- month ::= "0" nonZeroDigit | "10" | "11" | "12"
-- year ::= number
--
-- number ::= digit | number digit
-- digit ::= "0" | nonZeroDigit
-- nonZeroDigit ::= "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
--
-- monthName ::= "Jan" | "Feb" | "Mar" | "Apr" | "May" | "Jun" | "Jul" | "Aug" | "Sep" | "Oct" | "Nov" | "Dec"
-- @
--
-- Usage example:
--
-- >>> parse date "01.01.2012"
-- Parsed (Date (Day 1) (Month 1) (Year 2012)) (Input 10 "")
-- >>> parse date "12.12.2012"
-- Parsed (Date (Day 12) (Month 12) (Year 2012)) (Input 10 "")
-- >>> parse date "12-12-2012"
-- Parsed (Date (Day 12) (Month 12) (Year 2012)) (Input 10 "")
-- >>> parse date "Dec 12 2012"
-- Parsed (Date (Day 12) (Month 12) (Year 2012)) (Input 11 "")
-- >>> parse date "Jan 1 2012"
-- Parsed (Date (Day 1) (Month 1) (Year 2012)) (Input 10 "")
-- >>> parse date "Feb 31 2012"
-- Parsed (Date (Day 31) (Month 2) (Year 2012)) (Input 11 "")
-- >>> parse date "12/12/2012"
-- Failed [PosError 2 (Unexpected '/'),PosError 0 (Unexpected '1')]
--

nonZeroDigit :: Parser Char
nonZeroDigit = satisfy (`elem` ['1'..'9'])

digit :: Parser Char
digit = satisfy (`elem` ['0'..'9'])

digitsToInt :: String -> Int
digitsToInt = foldl (\acc c -> acc * 10 + digitToInt c) 0

year :: Parser Year
year = Year . digitsToInt <$> some digit

day :: Parser Day
day = Day 30 <$ string "30"
  <|> Day 31 <$ string "31"
  <|> Day . digitsToInt <$> ((:) <$> char '0' <*> ((:[]) <$> nonZeroDigit))
  <|> Day . digitsToInt <$> ((:) <$> char '1' <*> ((:[]) <$> digit))
  <|> Day . digitsToInt <$> ((:) <$> char '2' <*> ((:[]) <$> digit))

month :: Parser Month
month = choice
  [ Month 10 <$ string "10"
  , Month 11 <$ string "11"
  , Month 12 <$ string "12"
  , Month . digitToInt <$> ((\_ d -> d) <$> char '0' <*> nonZeroDigit)
  ]

usDay :: Parser Day
usDay =
      Day 30 <$ string "30"
  <|> Day 31 <$ string "31"
  <|> Day . digitsToInt <$> ((:) <$> char '1' <*> ((:[]) <$> digit))
  <|> Day . digitsToInt <$> ((:) <$> char '2' <*> ((:[]) <$> digit))
  <|> Day . digitToInt      <$> nonZeroDigit

monthName :: Parser Month
monthName = choice
  [ Month 1  <$ string "Jan"
  , Month 2  <$ string "Feb"
  , Month 3  <$ string "Mar"
  , Month 4  <$ string "Apr"
  , Month 5  <$ string "May"
  , Month 6  <$ string "Jun"
  , Month 7  <$ string "Jul"
  , Month 8  <$ string "Aug"
  , Month 9  <$ string "Sep"
  , Month 10 <$ string "Oct"
  , Month 11 <$ string "Nov"
  , Month 12 <$ string "Dec"
  ]

dotFormat :: Parser Date
dotFormat = Date
  <$> day   <* char '.'
  <*> month <* char '.'
  <*> year

hyphenFormat :: Parser Date
hyphenFormat = Date
  <$> day   <* char '-'
  <*> month <* char '-'
  <*> year

usFormat :: Parser Date
usFormat =
  flip Date
    <$> monthName <* char ' '
    <*> usDay     <* char ' '
    <*> year

date :: Parser Date
date = dotFormat
   <|> hyphenFormat
   <|> usFormat

