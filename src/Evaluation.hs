module Evaluation where
import AST

evaluate :: Expr -> Either Expr Float
evaluate (Constant x) = Right x

evaluate (Binary op l r) = do
  le <- evaluate l
  re <- evaluate r
  Right $ case op of
    Addition -> (le + re)
    Subtraction -> (le - re)
    Multiplication -> (le * re)
    Division -> (le / re)
    Assignment -> (le + re) -- TODO

evaluate (Unary op a) = do
  arg <- evaluate a
  Right $ case op of
    Negation -> (- arg)
    Exp -> (exp arg)
    Log -> (log arg)
    Sin -> (sin arg)
    Cos -> (cos arg)

evaluate (Scope expr) = evaluate expr >>= Left . Scope . Constant
evaluate (Identifier str) = Left $ Identifier str -- TODO
evaluate (Conditional cond true false) = do
  c <- evaluate cond
  if c > 0
    then do
      t <- evaluate true
      Left $ Constant $ t
    else do
      f <- evaluate false
      Left $ Constant $ f
    
