module Parsing where

import Control.Monad.State
import Text.Read (readMaybe)
import Data.Char
import Control.Applicative


-- Parsers
type ParseError = String
type Parser a = StateT String (Either ParseError) a

-- Error throwing
throwErr :: ParseError -> Parser a
throwErr msg = StateT $ \s -> Left msg

(<?>) :: Parser a -> ParseError -> Parser a
(StateT m) <?> msg = StateT $ \ s -> m s `mplus` Left msg


-- Lexical Parsers
readSpace :: Parser String
readSpace = many $ require isSpace

-- Reading tokens will remove all trailing whitespace
readToken :: Parser a -> Parser a
readToken p = p <* readSpace

readSymbol :: String -> Parser String
readSymbol s = readToken $ readString s

applyParser :: Parser a -> String -> Either ParseError a
applyParser p input = evalStateT (readSpace *> p) input


-- Helper combinators
              
require :: (Char -> Bool) -> Parser Char
require predicate = do
  token <- readAnyChar
  if predicate token
    then return token
    else mzero

readAnyChar :: Parser Char
readAnyChar = StateT $ \s -> case s of
  [] -> mzero
  (t:ts) -> return (t, ts)

readDigit :: Parser Char
readDigit = require isDigit

readChar :: Char -> Parser Char
readChar c = require (== c)

-- Recursive combinators

readString :: String -> Parser String
readString (x:xs) = do
  _ <- readChar x;
  _ <- readString xs;
  return (x:xs)
readString [] = return []

readNumber :: Parser String  
readNumber = readMany $ readDigit

readMany :: Parser a -> Parser [a]
readMany p = readMany1 p <|> mzero

readMany1 :: Parser a -> Parser [a]
readMany1 p = do
  x <- p
  xs <- readMany p
  return (x:xs)
  
-- Ex separateBy (,2,3) "," -> valid
separateBy :: Parser a -> Parser b -> Parser [a]
separateBy parser separator = (parser `separateBy1` separator) <|> mzero

-- Ex
-- separateBy (1, 2, 3) "," -> valid
-- (,2,3) -> invalid
separateBy1 :: Parser a -> Parser b -> Parser [a]
separateBy1 parser separator = do
  x <- parser
  xs <- many $ separator *> parser
  return (x:xs)

chain :: Parser a -> Parser (a -> a -> a) -> Parser a
chain parser operation = parser >>= rest
  where rest left = do {
          f <- operation;
          right <- parser;
          rest (f left right);
          } <|> return left

