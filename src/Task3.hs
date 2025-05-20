{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

module Task3 where

import Parser
import ParserCombinators
import Data.Char (toLower, isHexDigit, isDigit)
import Control.Applicative (many, some, (<|>))
import Data.List (intercalate)

-- | JSON representation
--
-- See <https://www.json.org>
--
data JValue =
    JObject [(String, JValue)]
  | JArray [JValue]
  | JString String
  | JNumber Double
  | JBool Bool
  | JNull
 deriving (Show, Eq)

-- | Parses JSON value
--
-- See full grammar at <https://www.json.org>
--
-- Usage example:
--
-- >>> parse json "{}"
-- Parsed (JObject []) (Input 2 "")
-- >>> parse json "null"
-- Parsed JNull (Input 4 "")
-- >>> parse json "true"
-- Parsed (JBool True) (Input 4 "")
-- >>> parse json "3.14"
-- Parsed (JNumber 3.14) (Input 4 "")
-- >>> parse json "{{}}"
-- Failed [PosError 0 (Unexpected '{'),PosError 1 (Unexpected '{')]
--
json :: Parser JValue
json = spaces *> jValue <* spaces

sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy p sep = (:) <$> p <*> many (sep *> p) <|> pure []

jValue :: Parser JValue
jValue =  parseNull
      <|> parseBool
      <|> parseNumber
      <|> parseString
      <|> parseArray
      <|> parseObject

parseNull :: Parser JValue
parseNull = JNull <$ string "null"

parseBool :: Parser JValue
parseBool = (JBool True  <$ string "true")
         <|> (JBool False <$ string "false")

parseNumber :: Parser JValue
parseNumber = JNumber . read <$> numberString
  where
    -- собираем [sign] ++ intPart ++ fracPart ++ expPart
    numberString :: Parser String
    numberString =
      (\s i f e -> s ++ i ++ f ++ e)
        <$> sign
        <*> intPart
        <*> fracPart
        <*> expPart

    sign     =      (:[]) <$> char '-' <|> pure ""
    intPart  = some (satisfy isDigit)
    fracPart = (:) <$> char '.' <*> some (satisfy isDigit) <|> pure ""
    expPart  =
          (\e s ds -> e : s ++ ds)
              <$> satisfy (\c -> c=='e' || c=='E')
              <*> ( (:[]) <$> (char '+' <|> char '-') <|> pure "" )
              <*> some (satisfy isDigit)
      <|> pure ""


parseString :: Parser JValue
parseString = JString . concat <$> between (char '"') (many chunk) (char '"')
  where
    chunk :: Parser String     
    chunk =  rawChar     
         <|> escaped 

    rawChar   = (:[]) <$> satisfy (\c -> c /= '"' && c /= '\\')
    escaped   = (:) <$> char '\\' <*> escapeTail 
    escapeTail =
          (:[]) <$> choice
            [ char '"' , char '\\' , char '/' , char 'b'
            , char 'f', char 'n' , char 'r' , char 't'
            ]
      <|> unicode


    unicode = (:) <$> char 'u' <*> count 4 (satisfy isHexDigit)

parseArray :: Parser JValue
parseArray = JArray
  <$> (char '[' *> spaces *> elements <* spaces <* char ']')
  where
    elements = sepBy (spaces *> jValue <* spaces) (spaces *> char ',' <* spaces)

parseObject :: Parser JValue
parseObject = JObject
  <$> (char '{' *> spaces *> members <* spaces <* char '}')
  where
    members = sepBy member (spaces *> char ',' <* spaces)
    member  = (,) <$> key <*> (spaces *> char ':' *> spaces *> jValue)
    key = char '"' *> (concat <$> many objChar) <* char '"'
    objChar = raw <|> esc
    raw = (:[]) <$> satisfy (\c -> c /= '"' && c /= '\\')
    esc = (:) <$> char '\\' <*> escapeTail
    escapeTail =
          (:[]) <$> choice
            [ char '"' , char '\\' , char '/' , char 'b'
            , char 'f', char 'n' , char 'r' , char 't'
            ]
      <|> unicode

    unicode = (:) <$> char 'u' <*> count 4 (satisfy isHexDigit)

-- * Rendering helpers

-- | Renders given JSON value as oneline string
render :: JValue -> String
render = concatMap readable . renderTokens
  where
    -- Adds some nice spacing for readability
    readable ":" = ": "
    readable "," = ", "
    readable s   = s

-- | Renders given JSON value as list of separate tokens ready for pretty printing
renderTokens :: JValue -> [String]
renderTokens JNull        = ["null"]
renderTokens (JBool b)    = [map toLower $ show b]
renderTokens (JNumber d)  = [show d]
renderTokens (JString s)  = ["\"" ++ s ++ "\""]
renderTokens (JArray xs)  = ["["] ++ intercalate [","] (map renderTokens xs) ++ ["]"]
renderTokens (JObject xs) = ["{"] ++ intercalate [","] (map renderPair xs) ++ ["}"]
 where
  renderPair :: (String, JValue) -> [String]
  renderPair (k, v) = ["\"" ++ k ++ "\""] ++ [":"] ++ renderTokens v

-- | Renders 'Parsed' or 'Failed' value as string
renderParsed :: Parsed JValue -> String
renderParsed (Parsed v _) = render v
renderParsed (Failed err) = show err

-- | Parses given file as JSON and renders result
renderJSONFile :: String -> IO String
renderJSONFile file = renderParsed <$> parseJSONFile file

-- | Parses given file as JSON
parseJSONFile :: String -> IO (Parsed JValue)
parseJSONFile file = parse json <$> readFile file
