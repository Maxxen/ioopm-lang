module Evaluation.StrictEval where
import AST
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Except
import Control.Monad.Identity
import qualified Data.HashMap.Lazy as H


type Var = String
type Environment = H.HashMap Var Expr
type EvalError = String

type Evaluator = StateT Environment (ExceptT EvalError (WriterT [String] Identity))

runEvaluator :: Evaluator a -> Environment -> Either EvalError (a, Environment)
runEvaluator ev env = case runIdentity $ runWriterT $ runExceptT $ runStateT ev env of
  (Right (r,s), log) -> Right (r, s)
  (Left err, log) -> Left err
  
type PartialEval = Evaluator (Either Expr Float)
type StrictEval = Evaluator Float

-- Strict evaluation

evaluate :: Expr -> StrictEval
evaluate (Constant x) = do
  tell [show x]
  return x

evaluate (Binary Assignment lhs (Identifier str)) = do
  val <- evaluate lhs
  modify $ (\env -> H.insert str (Constant val) env)
  return val

evaluate (Binary op lhs rhs) = do
  l <- evaluate lhs
  r <- evaluate rhs
  tell [show op]
  case op of
    Addition -> return (l + r)
    Subtraction -> return (l - r)
    Multiplication -> return (l * r)
    Division -> if r == 0
      then throwError "Division by 0!"
      else return (l / r)
    
evaluate (Unary op a) = do
  arg <- evaluate a
  case op of
    Negation -> return (- arg)
    Exp -> return (exp arg)
    Log -> if arg < 0
      then throwError "Logarithm is undefined for x < 0"
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
    Nothing -> throwError ("Unknown identifier: " ++ show str)


evaluate (Conditional cond true false) = do
  c <- evaluate cond
  if c > 0
    then do
      evaluate true
    else do
      evaluate false


