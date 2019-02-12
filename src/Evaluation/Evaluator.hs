module Evaluation.Evaluator where
import AST
import Parsing.Combinators (runParser)
import Data.Tuple
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Identity
import qualified Data.HashMap.Lazy as H


type VIdent = String
type FIdent = String
-- Fst = Variable hashmap
-- Snd = Function hashmap
type Environment = (H.HashMap VIdent Expr, H.HashMap FIdent Expr)

emptyEnv :: Environment
emptyEnv = (H.empty, H.empty)

insertFunc :: FIdent -> Expr -> Evaluator ()
insertFunc ident decl = modify $ (\(vars, funcs) -> (vars, H.insert ident decl funcs))

lookupFunc :: FIdent -> Evaluator (Maybe Expr)
lookupFunc ident = get >>= (return . H.lookup ident . snd)

insertVar :: VIdent -> Expr -> Evaluator ()
insertVar ident expr = modify $ (\(vars, funcs) -> (H.insert ident expr vars, funcs))

lookupVar :: VIdent -> Evaluator (Maybe Expr)
lookupVar ident = get >>= (return . H.lookup ident . fst)
  
type EvalError = String
type Evaluator = StateT Environment (ExceptT EvalError Identity)
type Evaluation = Evaluator Expr

runEvaluator ::   Evaluation -> Environment -> Either EvalError (Expr, Environment)
runEvaluator ev = runIdentity . runExceptT . runStateT ev

evaluate :: Expr -> Evaluation
evaluate (Constant x) = return (Constant x)

evaluate (Block l) = do
  env <- get
  v <- foldl1 (>>) (fmap evaluate l)
  put env
  return v

evaluate (Scope expr) = do
  env <- get -- Copy environment state
  val <- evaluate expr
  put env -- Reset environment state
  return val


evaluate (Binary Assignment lhs (Identifier ident)) = do
  val <- evaluate lhs
  insertVar ident val
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

evaluate (Conditional condExp thenExp elseExp) = do
  cond <- evaluate condExp
  case cond of
    (Constant bool ) ->
      if bool /= 0
      then evaluate thenExp
      else evaluate elseExp
    _ -> return $ Conditional cond thenExp elseExp

evaluate f@(FunctionDecl ident params body) = do
  insertFunc ident f
  return f

evaluate (FunctionCall ident args) = do
  f <- lookupFunc ident
  case f of
    Nothing -> throwError $ "Unknown function: " ++ ident
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


evaluate (Identifier ident) = do
  v <- lookupVar ident
  case v of
    Just expr -> evaluate expr
    Nothing -> return (Identifier ident)
