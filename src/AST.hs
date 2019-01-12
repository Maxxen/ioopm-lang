module AST where
import Parsing
import Control.Applicative
import Control.Monad

data Expr
  = Quit
  | Vars
  | Binary BOp Expr Expr
  | Unary UOp Expr
  | Scope Expr
  | Constant Float
  | Identifier String
  | Conditional Expr Expr Expr
  | Function [Expr] Expr
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
parse = topLevel

topLevel :: Parser Expr
topLevel = command <|> assignment

command :: Parser Expr
command = do
  var <- readSymbol "quit" <|> readSymbol "vars"
  case var of
    "quit" -> return Quit
    "vars" -> return Vars
    _ -> mzero

assignment :: Parser Expr
assignment = do {
  left <- expression;
  readSymbol "=";
  right <- rhs;
  return $ Binary Assignment left right;
  } <|> expression


rhs :: Parser Expr
rhs = chain identifier (readSymbol "=" *> return (Binary Assignment) )

expression :: Parser Expr
expression = chain term operation
  where
    operation = do
      op <- readSymbol "+" <|> readSymbol "-"
      return $ case op of
        "+" -> Binary Addition
        "-" -> Binary Subtraction

term :: Parser Expr
term = chain primary operation
  where
    operation = do
      op <- readSymbol "*" <|> readSymbol "/"
      return $ case op of
        "*" ->  Binary Multiplication
        "/" ->  Binary Division
        
primary :: Parser Expr
primary = readSymbol "(" *> assignment  <* readSymbol ")"
  <|> unary
  <|> readConstant
  -- <|> identifier

readConstant :: Parser Expr
readConstant = do
  -- This could probably be done more efficiently, add a primitive "read any string" combinator?
  -- My many and many1 doesnt work. In this case I use some from Alternative instead. which corresponds to many1. Applicative also has many which is many
  token <- readToken $ some $ readDigit
  return $ Constant (read token :: Float)

    -- ReadAnyString doesnt work!, try replace with ReadNumber
unary :: Parser Expr
unary = do
  func <- foldl1 (<|>) $ map readSymbol ["-", "exp", "log", "sin", "cos"]
  arg <- primary
  return $ case func of
    "-" -> Unary Negation arg
    "exp" -> Unary Exp arg
    "log" -> Unary Log arg
    "sin" -> Unary Sin arg
    "cos" -> Unary Cos arg
    
identifier :: Parser Expr
identifier = do
  t <- readToken readAnyChar
  return $ Identifier [t]


