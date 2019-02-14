{-# OPTIONS_GHC -Wall #-}

module AST where
import Parsing.Combinators
import Debug.Trace

data Expr
  = Block [Expr]
  | Binary BOp Expr Expr
  | Unary UOp Expr
  | Scope Expr
  | Constant Float
  | Identifier String
  | Conditional Expr Expr Expr
  | FunctionCall String [Expr]
  | FunctionDecl String [String] Expr
  deriving(Eq, Show)

data BOp
  = Assignment
  | Addition
  | Subtraction
  | Multiplication
  | Division
  deriving (Eq)

data UOp
  = Negation
  | Exp
  | Log
  | Sin
  | Cos
  deriving (Eq, Show)

instance Show BOp where
  show Assignment = "="
  show Addition = "+"
  show Subtraction = "-"
  show Multiplication = "*"
  show Division = "/"

parse :: Parser Expr
parse = let b = traceShow "Block:" (block topLevel)
        in b

topLevel :: Parser Expr
topLevel = functionDecl <|> assignment

block :: Parser Expr -> Parser Expr
block p = many p >>= return . Block

assignment :: Parser Expr
assignment = chainUp expression (symbol "=" >> return (Binary Assignment)) identifier

expression :: Parser Expr
expression = chain term operation
  where
    operation
       = (symbol "+" >> return (Binary Addition))
      <|> (symbol "-" >> return (Binary Subtraction))

term :: Parser Expr
term = chain primary operation
  where
    operation
       = (symbol "*" >> return (Binary Multiplication))
      <|>(symbol "/" >> return (Binary Division))

primary :: Parser Expr
primary = constant
  <|> scope (parens assignment)
  <|> conditional
  <|> unary
  <|> functionCall
  <|> identifier


constant :: Parser Expr
constant = traceShow "Constant" $ (token $ many1 $ digit) >>= return . Constant . (read :: String -> Float)

scope :: Parser Expr -> Parser Expr
scope p = p >>= return . Scope

unary :: Parser Expr
unary
   =  (symbol "-" >> (Unary Negation) <$> primary)
  <|> (symbol "exp" >> (Unary Exp) <$> primary)
  <|> (symbol "log" >> (Unary Log) <$> primary)
  <|> (symbol "sin" >> (Unary Sin) <$> primary)
  <|> (symbol "cos" >> (Unary Cos) <$> primary)

conditional :: Parser Expr
conditional = do
  _ <- symbol "if"
  condExp <- parens expression
  thenExp <- curly $ block assignment
  _ <- symbol "else"
  elseExp <- curly $ block assignment
  return $ Conditional condExp thenExp elseExp
  
            
identifier :: Parser Expr
identifier = identif >>= return . Identifier

functionCall :: Parser Expr
functionCall = do
  ident <- identif
  args <- parens $ separateBy1 assignment (symbol ",")
  return $ FunctionCall ident args

  
functionDecl :: Parser Expr
functionDecl = do
  _ <- symbol "function"
  name <- identif
  args <- parens $ separateBy identif (symbol ",")
  body <- curly $ block assignment
  return $ FunctionDecl name args body
