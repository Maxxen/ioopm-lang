module AST where
import Parsing.Combinators
import Control.Applicative
import Control.Monad
import Control.Monad.Reader

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
parse = block (topLevel) <?> "ERROR"

topLevel :: Parser Expr
topLevel = functionDecl <|> assignment

block :: Parser Expr -> Parser Expr
block p = many p >>= return . Block

assignment :: Parser Expr
assignment = chainUp expression (readSymbol "=" >> return (Binary Assignment)) identifier

expression :: Parser Expr
expression = chain term operation
  where
    operation
       = (readSymbol "+" >> return (Binary Addition))
      <|> (readSymbol "-" >> return (Binary Subtraction))

term :: Parser Expr
term = chain primary operation
  where
    operation
       = (readSymbol "*" >> return (Binary Multiplication))
      <|>(readSymbol "/" >> return (Binary Division))

primary :: Parser Expr
primary = constant
  <|> scope (parens assignment)
  <|> conditional
  <|> unary
  <|> functionCall
  <|> identifier
  <?> "error, expected constant or identifier!"

constant :: Parser Expr
constant = (readToken $ some $ readDigit) >>= return . Constant . (read :: String -> Float)

scope :: Parser Expr -> Parser Expr
scope p = p >>= return . Scope

unary :: Parser Expr
unary
   =  (readSymbol "-" >> (Unary Negation) <$> primary)
  <|> (readSymbol "exp" >> (Unary Exp) <$> primary)
  <|> (readSymbol "log" >> (Unary Log) <$> primary)
  <|> (readSymbol "sin" >> (Unary Sin) <$> primary)
  <|> (readSymbol "cos" >> (Unary Cos) <$> primary)

conditional :: Parser Expr
conditional = do
  readSymbol "if"
  condExp <- parens expression
  thenExp <- curly $ block assignment
  readSymbol "else"
  elseExp <- curly $ block assignment
  return $ Conditional condExp thenExp elseExp
  
            
identifier :: Parser Expr
identifier = readIdentifier >>= return . Identifier

functionCall :: Parser Expr
functionCall = do
  ident <- readIdentifier
  args <- parens $ separateBy1 assignment (readSymbol ",")
  return $ FunctionCall ident args

  
functionDecl :: Parser Expr
functionDecl = do
  readSymbol "function"
  name <- readIdentifier
  args <- parens $ separateBy readIdentifier (readSymbol ",")
  body <- curly $ block assignment
  return $ FunctionDecl name args body
