module Evaluation.Evaluator where
import AST
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Trans.Either
import qualified Data.HashMap.Lazy as H


type Var = String
type Environment = H.HashMap Var Expr
type EvalError = String

type Evaluator = EitherT Expr (StateT Environment (ExceptT EvalError Identity))

runEvaluator :: Evaluator a -> Environment -> Either EvalError (a, Environment)
runEvaluator ev env = case runIdentity $ runExceptT $ runStateT (runEitherT ev) env of
  (Left err) -> Left err
  (Right c) -> case c of
                 (Left expr, s) -> Left "EXPRESSION NOT FULLY EVALUATED"
                 ((Right val), s) -> Right (val, s)

  
type Evaluation = Evaluator Float

-- Strict evaluation

evaluate :: Expr -> Evaluation
evaluate (Constant x) = do
  return x

evaluate (Binary Assignment lhs (Identifier str)) = do
  val <- evaluate lhs
  modify $ (\env -> H.insert str (Constant val) env)
  return val

evaluate (Binary op lhs rhs) = do
  l <- evaluate lhs
  r <- evaluate rhs
  case op of
    Addition -> return $ l + r
    Subtraction -> return $ l - r
    Multiplication -> return $ l * r
    Division -> if r == 0
      then lift $ lift $ throwError "Division by 0!"
      else return $ l / r
    
evaluate (Unary op a) = do
  arg <- evaluate a
  case op of
    Negation -> return (- arg)
    Exp -> return (exp arg)
    Log -> if arg < 0
      then lift $ lift $ throwError "Logarithm is undefined for x < 0"
      else return (log arg)
    Sin -> return (sin arg)
    Cos -> return (cos arg)

evaluate (Scope expr) = do
  env <- get -- Copy environment state
  val <- evaluate expr
  put env -- Reset environment state
  return val

evaluate (Identifier str) = do
  env <- get
  case H.lookup str env of
    Just expr -> evaluate expr
    Nothing -> throwError (Identifier str)
    --Slight abuse of throwError, but how else am i supposed to return Left?

evaluate (Conditional cond true false) = do
  c <- evaluate cond
  if c > 0
    then do
      evaluate true
    else do
      evaluate false


