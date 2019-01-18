module Evaluation.PartialEval where
import AST
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Except
import Control.Monad.Identity
import qualified Data.HashMap.Lazy as H

-- Partial Evaluation

evalPartial :: Expr -> Either Expr Float
evalPartial (Constant x) = Right x

evalPartial (Binary op l r) = do
  le <- evalPartial l
  re <- evalPartial r
  Right $ case op of
    Addition -> (le + re)
    Subtraction -> (le - re)
    Multiplication -> (le * re)
    Division -> (le / re)
    Assignment -> (le + re) -- TODO

evalPartial (Unary op a) = do
  arg <- evalPartial a
  Right $ case op of
    Negation -> (- arg)
    Exp -> (exp arg)
    Log -> (log arg)
    Sin -> (sin arg)
    Cos -> (cos arg)

evalPartial (Scope expr) = evalPartial expr >>= Left . Scope . Constant
evalPartial (Identifier str) = Left $ Identifier str -- TODO
evalPartial (Conditional cond true false) = do
  c <- evalPartial cond
  if c > 0
    then do
      t <- evalPartial true
      Left $ Constant $ t
    else do
      f <- evalPartial false
      Left $ Constant $ f
    
