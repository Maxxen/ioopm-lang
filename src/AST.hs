module AST where
import Parsing.Combinators
import Control.Applicative
import Control.Monad

data Expr
  = Block [Expr]
  | Quit
  | Vars
  | Binary BOp Expr Expr
  | Unary UOp Expr
  | Scope Expr
  | Constant Float
  | Identifier String
  | Conditional Expr Expr Expr
  | FunctionCall String Expr
  | FunctionDecl String [String] Expr
  deriving(Eq, Show)

data BOp
  = Assignment
  | Addition
  | Subtraction
  | Multiplication
  | Division
  deriving (Eq, Show)

data UOp
  = Negation
  | Exp
  | Log
  | Sin
  | Cos
  deriving (Eq, Show)


parse :: Parser Expr
parse = topLevel <?> "ERROR"

topLevel :: Parser Expr
topLevel = command <|> block (functionDecl <|> assignment)

command :: Parser Expr
command = (readSymbol "quit" >> return Quit) <|> (readSymbol "vars" >> return Vars)

block :: Parser Expr -> Parser Expr
block p = many p >>= return . Block

assignment :: Parser Expr
assignment = chainUp expression (readSymbol "=" >> return (Binary Assignment)) identifier

expression :: Parser Expr
expression = chain term operation
  where
    operation =
      (readSymbol "+" >> return (Binary Addition)) <|>
      (readSymbol "-" >> return (Binary Subtraction))

term :: Parser Expr
term = chain primary operation
  where
    operation =
      (readSymbol "*" >> return (Binary Multiplication)) <|>
      (readSymbol "/" >> return (Binary Division))

primary :: Parser Expr
primary = readConstant
  <|> parens assignment
  <|> unary
  <|> functionCall
  <|> identifier
  <?> "error, expected constant or identifier!"
  
readConstant :: Parser Expr
readConstant = do
  -- This could probably be done more efficiently, add a primitive "read any string" combinator?
  -- My many and many1 doesnt work. In this case I use some from Alternative instead. which corresponds to many1. Applicative also has many which is many
  token <- readToken $ some $ readDigit
  return $ Constant (read token :: Float)

  -- ReadAnyString doesnt work!, try replace with ReadNumber

unary ::Parser Expr
unary
   =  (readSymbol "-" >> (Unary Negation) <$> primary)
  <|> (readSymbol "exp" >> (Unary Exp) <$> primary)
  <|> (readSymbol "log" >> (Unary Log) <$> primary)
  <|> (readSymbol "sin" >> (Unary Sin) <$> primary)
  <|> (readSymbol "cos" >> (Unary Cos) <$> primary)
  
    
identifier :: Parser Expr
identifier = do
  str <- readIdentifier
  return $ Identifier str


functionCall :: Parser Expr
functionCall = do
  ident <- readIdentifier
  args <- parens $ separateBy1 assignment (readSymbol ",") >>= return . Block
  return $ FunctionCall ident args

  
functionDecl :: Parser Expr
functionDecl = do
  readSymbol "function"
  name <- readIdentifier
  args <- parens $ separateBy readIdentifier (readSymbol ",")
  body <- curly $ block assignment
  return $ FunctionDecl name args body
