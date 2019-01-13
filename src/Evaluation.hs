module Evaluation where
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

emptyEnv :: H.HashMap Var Expr
emptyEnv = H.empty

-- Strict evaluation

evalStrict :: Expr -> StrictEval
evalStrict (Constant x) = do
  tell [show x]
  return x

evalStrict (Binary Assignment lhs (Identifier str)) = do
  val <- evalStrict lhs
  modify $ (\env -> H.insert str (Constant val) env)
  return val

evalStrict (Binary op lhs rhs) = do
  l <- evalStrict lhs
  r <- evalStrict rhs
  tell [show op]
  case op of
    Addition -> return (l + r)
    Subtraction -> return (l - r)
    Multiplication -> return (l * r)
    Division -> if r == 0
      then throwError "Division by 0!"
      else return (l / r)
    
evalStrict (Unary op a) = do
  arg <- evalStrict a
  case op of
    Negation -> return (- arg)
    Exp -> return (exp arg)
    Log -> if arg < 0
      then throwError "Logarithm is undefined for x < 0"
      else return (log arg)
    Sin -> return (sin arg)
    Cos -> return (cos arg)

evalStrict (Scope expr) = do
  env <- get -- Copy environment state
  val <- evalStrict expr
  put env -- Reset environment state
  return val


evalStrict (Identifier str) = do
  env <- get
  case H.lookup str env of
    Just expr -> evalStrict expr
    Nothing -> throwError ("Unknown identifier: " ++ show str)


evalStrict (Conditional cond true false) = do
  c <- evalStrict cond
  if c > 0
    then do
      evalStrict true
    else do
      evalStrict false



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
    
