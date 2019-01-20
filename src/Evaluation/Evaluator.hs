module Evaluation.Evaluator where
import AST
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Except
import Control.Monad.Identity
import qualified Data.HashMap.Lazy as H

type Var = String
type Environment = H.HashMap Var Expr
type EvalError = String

type Evaluator = StateT Environment (ExceptT EvalError Identity)
type Evaluation = Evaluator Expr

runEvaluator :: Evaluation -> Environment -> Either EvalError (Float, Environment)
runEvaluator ev env = case runIdentity $ runExceptT $ runStateT ev env of
  (Left err) -> Left err
  (Right (r, s)) -> case r of
                      (Constant x) -> Right(x, s)
                      (partialExpr) -> Left $ "PARTIAL EVALUATION: " ++ show partialExpr
                      


evaluate :: Expr -> Evaluation
evaluate (Constant x) = return (Constant x)

evaluate (Binary Assignment lhs (Identifier str)) = do
  val <- evaluate lhs
  modify $ (\env -> H.insert str lhs env)
  return val

evaluate (Binary op lhs rhs) = do
  l <- evaluate lhs
  r <- evaluate rhs
  case (l, r) of
    (Constant lc, Constant rc) -> reduce op lc rc  
    (le, re) -> return $ Binary op le re
    
  where
    reduce :: BOp -> Float -> Float -> Evaluation
    reduce (Addition) l r = return $ Constant $ l + r
    reduce (Subtraction) l r = return $ Constant $ l - r
    reduce (Multiplication) l r = return $ Constant $ l * r
    reduce (Division) l 0 = lift $ throwError "Division by 0!"
    reduce (Division) l r = return $ Constant $ l / r
              

evaluate (Unary op a) = do
  arg <- evaluate a
  case arg of
    (Constant ac) -> reduce op ac
    (ae) -> return $ Unary op ae

  where
    reduce :: UOp -> Float -> Evaluation
    reduce (Negation) arg = return $ Constant (- arg)
    reduce (Sin) arg = return $ Constant (sin arg)
    reduce (Cos) arg = return $ Constant (cos arg)
    reduce (Exp) arg = return $ Constant (exp arg)
    reduce (Log) arg = if arg < 0
                         then lift $ throwError "Logarithm is undefined for x < 0"
                         else return $ Constant (log arg)
   
evaluate (Scope expr) = do
  env <- get -- Copy environment state
  val <- evaluate expr
  put env -- Reset environment state
  return val

evaluate ident@(Identifier str) = do
  env <- get
  case H.lookup str env of
    Just expr -> evaluate expr
    Nothing -> return ident
