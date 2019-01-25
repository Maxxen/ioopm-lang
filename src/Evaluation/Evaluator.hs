module Evaluation.Evaluator where
import AST
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Identity
import qualified Data.HashMap.Lazy as H

type Var = String
type Environment = H.HashMap Var Expr
type EvalError = String

type Evaluator = StateT Environment (ExceptT EvalError Identity)
type Evaluation = Evaluator Expr

runEvaluator ::  Environment -> Evaluation -> Either EvalError (Float, Environment)
runEvaluator env ev = case runIdentity $ runExceptT $ runStateT ev env of
  (Left err) -> Left err
  (Right (r, s)) -> case r of
                      (Constant x) -> Right(x, s)
                      (partialExpr) -> Left $ "PARTIAL EVALUATION: " ++ show partialExpr


evaluate :: Expr -> Evaluation
evaluate (Constant x) = return (Constant x)
evaluate (Block l) = foldl1 (>>) (fmap evaluate l)
evaluate (Binary Assignment lhs (Identifier str)) = do
  val <- evaluate lhs
  modify $ H.insert str val
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

evaluate f@(FunctionDecl name params body) = do
  modify $ H.insert name f
  return f

evaluate (FunctionCall name (Block args)) = do
  env <- get
  case H.lookup name env of
    Nothing -> throwError $ "Unknown function: " ++ name
    Just (FunctionDecl _ params (Block body)) -> do
      assignedArgs <- assign args params
      evaluate $ Block $ assignedArgs ++ body
      

  where
    assign :: [Expr] -> [String] -> Evaluator [Expr]
    assign a p
      | length a < length p = throwError "Missing arguments!"
      | length a > length p = throwError "Too many arguments!"
      | length a == length p = return $
        zipWith (\expr ident -> Binary Assignment expr (Identifier ident)) a p

        
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
