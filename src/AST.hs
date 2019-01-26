module AST where
import Parsing.Combinators
import Control.Applicative
import Control.Monad
import Data.List
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
  deriving(Eq)

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



instance Show Expr where
  show e = drawAST 0 e

drawAST :: Int -> Expr -> String
drawAST d (Block lines) =
  let nd = d + 1
  in "{\n" ++ intercalate "\n" (map (indent nd . drawAST nd) lines) ++ "\n" ++ (indent d "}\n")
drawAST d (Binary op l r) = (drawAST d l) ++ " " ++ show op ++ " " ++ (drawAST d r)
drawAST d (Unary op argument) = show op ++ "(" ++ (drawAST d argument) ++ ")"
drawAST d (Scope expression) = "{" ++ (drawAST d expression) ++ "}"
drawAST d (Constant float) = show float
drawAST d (Identifier ident) = ident
drawAST d (Conditional i t e) = "TODO"
drawAST d (FunctionCall ident arguments) =
  ident ++ "(" ++ intercalate ", " (map (drawAST d) arguments) ++ ")"
  
drawAST d (FunctionDecl ident params (Block body)) =
  let nd = d + 1
  in "function " ++ ident ++ "(" ++ intercalate ", " params ++ ")" ++
     "{\n" ++ intercalate "\n" (map (indent nd . drawAST nd) body) ++ "\n" ++ indent d "}\n"
    
indent :: Int -> String -> String
indent n str = (concat $ replicate n "  ") ++ str

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
  args <- parens $ separateBy1 assignment (readSymbol ",")
  return $ FunctionCall ident args

  
functionDecl :: Parser Expr
functionDecl = do
  readSymbol "function"
  name <- readIdentifier
  args <- parens $ separateBy readIdentifier (readSymbol ",")
  body <- curly $ block assignment
  return $ FunctionDecl name args body
