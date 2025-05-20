{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

{-# OPTIONS_GHC -Wno-unused-top-binds #-}
-- The above pragma temporarily disables warnings about Parser constructor and runParser not being used
{-# LANGUAGE InstanceSigs #-}

module Parser
  ( -- * Important note
    -- 
    -- | The implementation of 'Parser' is intentionally
    -- hidden to other modules to encourage use of high level
    -- combinators like 'satisfy' and the ones from 'ParserCombinators'
    Parser
  , parse
  , parseMaybe
  , satisfy
  , Error(..)
  , Position(..)
  , Parsed(..)
  , Input
  ) where
import Data.List (nub)
import Control.Applicative
import Prelude hiding (pred)

-- | Value annotated with position of parsed input starting from 0
data Position a = Position Int a
 deriving (Show, Eq)

-- | Parser input encapsulating remaining string to be parsed with current position
type Input = Position String

-- | Parsing error
data Error =
    Unexpected Char -- ^ Unexpected character
  | EndOfInput      -- ^ Unexpected end of input
 deriving (Show, Eq)

-- | Parsing result of value of type @a@
data Parsed a =
    Parsed a Input           -- ^ Successfully parsed value of type @a@ with remaining input to be parsed
  | Failed [Position Error]  -- ^ Failed to parse value of type @a@ with accumulated list of errors
 deriving Show

-- | Parser of value of type @a@
newtype Parser a = Parser { runParser :: Input -> Parsed a }

-- | Runs given 'Parser' on given input string
parse :: Parser a -> String -> Parsed a
parse (Parser p) str = p (Position 0 str)

-- | Runs given 'Parser' on given input string with erasure of @Parsed a@ to @Maybe a@
parseMaybe :: Parser a -> String -> Maybe a
parseMaybe pr s = case parse pr s of
  Parsed x _ -> Just x
  Failed _   -> Nothing

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f (Parser p) = Parser $ \inp ->
    case p inp of
      Parsed x rest -> Parsed (f x) rest
      Failed errs   -> Failed errs

instance Applicative Parser where
  pure :: a -> Parser a
  pure x = Parser $ \inp -> Parsed x inp

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  Parser pf <*> Parser pa = Parser $ \inp ->
    case pf inp of
      Parsed f inp' -> case pa inp' of
        Parsed x inp'' -> Parsed (f x) inp''
        Failed errsA   -> Failed errsA
      Failed errsF -> Failed errsF

instance Alternative Parser where
  empty :: Parser a
  empty = Parser $ \_ -> Failed []
  -- Note: when both parsers fail, their errors are accumulated and *deduplicated* to simplify debugging
  (<|>) :: Parser a -> Parser a -> Parser a
  Parser p1 <|> Parser p2 = Parser $ \inp ->
    case p1 inp of
      Parsed x rest -> Parsed x rest
      Failed e1     -> case p2 inp of
        Parsed y rest' -> Parsed y rest'
        Failed e2      -> Failed (nub (e1 ++ e2))

-- | Parses single character satisfying given predicate
--
-- Usage example:
--
-- >>> parse (satisfy (>= 'b')) "foo"
-- Parsed 'f' (Position 1 "oo")
-- >>> parse (satisfy (>= 'b')) "bar"
-- Parsed 'b' (Position 1 "ar")
-- >>> parse (satisfy (>= 'b')) "abc"
-- Failed [Position 0 (Unexpected 'a')]
-- >>> parse (satisfy (>= 'b')) ""
-- Failed [Position 0 EndOfInput]
--
satisfy :: (Char -> Bool) -> Parser Char
satisfy pred = Parser $ \(Position pos s) ->
  case s of
    (c:cs)
      | pred c    -> Parsed c (Position (pos + 1) cs)
      | otherwise -> Failed [Position pos (Unexpected c)]
    []            -> Failed [Position pos EndOfInput]
