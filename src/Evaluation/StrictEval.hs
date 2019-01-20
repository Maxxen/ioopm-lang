module Evaluation.StrictEval where
import AST
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Except
import Control.Monad.Identity
import qualified Data.HashMap.Lazy as H



type EvalError = String
type Var = String
type Environment = H.HashMap Var Expr
type Evaluator = ReaderT Environment (ExceptT EvalError (WriterT [String] Identity))

runEvaluator :: Evaluator a -> Environment -> Either EvalError a
runEvaluator ev env = case runIdentity $ runWriterT $ runExceptT $ runReaderT ev env of
  (Right r, log) -> Right r
  (Left err, log) -> Left err  

type StrictEval = Evaluator Float

-- Strict evaluation

evaluate :: Expr -> StrictEval
evaluate (Constant x) = do
  tell [show x]
  return x

evaluate (Binary Assignment lhs (Identifier str)) = do
  val <- evaluate lhs
  local (\env -> H.insert str (Constant val) env) (evaluate lhs)
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
  val <- local id $ evaluate expr
  return val


evaluate (Identifier str) = do
  env <- ask
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


