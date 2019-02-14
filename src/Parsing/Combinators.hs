{-# OPTIONS_GHC -Wall #-}
module Parsing.Combinators where
import Data.Char
import Debug.Trace
-- (Column, Row)
type Position = (Int, Int)
data Input = Input {chars :: String, pos :: Position}

type ParseError = String

data Consumed a
  = Consumed (Reply a)
  | Empty (Reply a)

instance Functor Consumed where
  fmap f (Consumed r) = Consumed (fmap f r)
  fmap f (Empty r) = Consumed (fmap f r)

data Reply a
  = Ok a Input
  | Error ParseError

instance Functor Reply where
  fmap f (Ok a input) = Ok (f a) input
  fmap _ (Error e) = (Error e)

data Parser a = Parser (Input -> Consumed a)

runParser :: Parser a -> String -> Either ParseError a
runParser (Parser p) input = case p (Input input (0, 0)) of
  Consumed (Ok v _) -> Right v
  Empty (Ok v _) -> Right v
  Consumed (Error err) -> Left err
  Empty (Error err) -> Left err
    
unwrap :: Parser a -> (Input -> Consumed a)
unwrap (Parser p) = p

instance Functor Parser where
  fmap f (Parser action) = Parser $ fmap f . action

instance Applicative Parser where
  pure a = Parser $ \input -> Empty (Ok a (input))
  
  (Parser p) <*> q = Parser $ \input -> case (p input) of
    Consumed reply -> case reply of
      Ok f rest -> unwrap (f <$> q) rest
      Error err -> Consumed $ Error err  
    Empty reply -> case reply of
      Ok f rest -> unwrap (f <$> q) rest
      Error err -> Empty $ Error err
                           
instance Monad Parser where
  return = pure
  
  (Parser p) >>= f
    = Parser $ \input -> case p input of
    Empty reply1
      -> case reply1 of
           Ok x rest -> unwrap (f x) rest
           Error err -> Empty $ Error err
           
    Consumed reply1
      -> Consumed $
         case reply1 of
           Ok x rest
             -> case unwrap (f x) rest of
                  Consumed reply2 -> reply2
                  Empty reply2 -> reply2
           Error err -> Error err


(<|>) :: Parser a -> Parser a -> Parser a
(Parser p) <|> (Parser q) = Parser $ \input -> case p input of
  Empty (Error _) -> (q input)
  Empty ok -> case (q input) of
                Empty _ -> Empty ok
                consumed -> consumed
  consumed -> consumed  
  
many1 :: Parser a -> Parser [a]
many1 p = do
  x  <- p;
  xs <- (many1 p <|> return [])
  return (x:xs)

many :: Parser a -> Parser [a]
many p = many1 p <|> return []

-- Error throwing
throwErr :: String -> Parser a
throwErr msg = Parser $ \input -> Empty $ Error $ (show (pos input)) ++ ": " ++ show msg

-- Lexical Parsers
satisfy :: (Char -> Bool) -> Parser Char
satisfy test = Parser $ \input -> case chars input of
  [] -> Empty $ Error $ traceShow "EOF" "End of file!"
  (c:cs) | traceShow c $ test c -> 
             let (col, row) = pos input
             in traceShow "Consumed" Consumed $ Ok c (Input cs (col, row + 1))
         | otherwise -> Empty $ Error $ "Unexpected: '" ++ [c] ++ "' " ++ show (pos input)

space :: Parser String
space = many $ satisfy isSpace

-- Reading tokens will remove all trailing whitespace
token :: Parser a -> Parser a
token p = p <* space

symbol :: String -> Parser String
symbol s = token $ string s
    
digit :: Parser Char
digit = traceShow "Digit" satisfy isDigit
char :: Char -> Parser Char
char c = satisfy (== c)

-- Recursive combinators

string :: String -> Parser String
string (x:xs) = do
  _ <- char x;
  _ <- string xs;
  return (x:xs)
string [] = return []

number :: Parser String  
number = many $ digit

identif :: Parser String
identif = token $ many1 $ satisfy isAlphaNum  


-- Ex separateBy (,2,3) "," -> valid
separateBy :: Parser a -> Parser b -> Parser [a]
separateBy parser separator = (parser `separateBy1` separator) <|> return []

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

parens :: Parser a -> Parser a
parens p = symbol "(" *> p <* symbol ")"

curly :: Parser a -> Parser a
curly p = symbol "{" *> p <* symbol "}"
