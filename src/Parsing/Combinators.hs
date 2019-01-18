module Parsing.Combinators where

import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Identity
import Text.Read (readMaybe)
import Data.Char
import Control.Applicative

-- Parsers
--type ParseError = String
data ParseError = Unexpected String | Expected String
  deriving(Show)

instance Monoid ParseError where
  mempty = Unexpected ""
  
instance Semigroup ParseError where
  e <> (Unexpected _) = e
  e <> f = f

data Input = Input {input :: String, column :: Int}
type Parser a = StateT Input (Except ParseError) a

-- Error throwing
throwErr :: String -> Parser a
throwErr msg = StateT $ \s -> throwError $ Expected $ (show (column s)) ++ ": " ++ show msg

-- Provide more context in case of Unexpected errors
(<?>) :: Parser a -> String -> Parser a
p <?> msg = (mapStateT . mapExceptT . fmap) f p
  where
    f (Left (Unexpected err)) = Left $ Expected $ err ++ "\n" ++ msg
    f other = other

--(StateT m) <?> msg = StateT $ \s -> case runIdentity $ runExceptT (m s) of
--  Right val -> return val
--  Left (Unexpected err) -> throwError $ Expected $ (show (column s)) ++ ": " ++ msg
--  Left err -> throwError err

-- Lexical Parsers
readSpace :: Parser String
readSpace = many $ require isSpace

-- Reading tokens will remove all trailing whitespace
readToken :: Parser a -> Parser a
readToken p = p <* readSpace

readSymbol :: String -> Parser String
readSymbol s = readToken $ readString s

applyParser :: Parser a -> String -> Either ParseError a
applyParser p input = runIdentity $ runExceptT $ evalStateT (readSpace *> p) (Input input 0) 


-- Helper combinators
              
require :: (Char -> Bool) -> Parser Char
require predicate = do
  token <- readAnyChar
  if predicate token
    then return token
    else throwError $ Unexpected $ " " ++ [token]

readAnyChar :: Parser Char
readAnyChar = StateT $ \s -> case input s of
  [] -> throwError $ Unexpected "end of file!"
  (t:ts) -> return (t, Input ts (column s + 1))

    
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


{--
Chain expressions "left":
  op
  / \
 p   op
     / \
    p   p
--}
chain :: Parser a -> Parser (a -> a -> a) -> Parser a
chain parser operation = parser >>= rest
  where rest left = do {
          f <- operation;
          right <- parser;
          rest (f left right);
          } <|> return left

{-
Chain expression "upwards"
     op
    /  \
   op   p
  /  \
 s    p
-}
chainUp :: Parser a -> Parser (a -> a -> a) -> Parser a -> Parser a
chainUp start operation parser = do {
  s <- start;
  op <- operation;
  p <- parser;
  chainUp (return (op s p)) operation parser;
  } <|> start
