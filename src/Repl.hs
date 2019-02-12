module Repl where
import AST
import Evaluation.Evaluator
import Evaluation.PrettyPrinter
import Parsing.Combinators
import Control.Monad.Writer
import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Trans.Maybe

type Repl = MaybeT (WriterT String Identity)

runRepl :: String -> (Maybe (Expr, Environment), String)
runRepl = runIdentity . runWriterT . runMaybeT . (\input -> tryParse input >>= tryReduce)


tryParse :: String -> Repl Expr
tryParse input = case runParser parse input of
  (Right result) -> do
    tell $ "\nParsed: " ++ input ++ "\nAs: " ++ show result
    return $ result
  (Left err) -> do
    tell $ show err ++ "\nWhen trying to parse: " ++ input
    mzero

tryReduce :: Expr -> Repl (Expr, Environment)
tryReduce input = case runEvaluator (evaluate input) emptyEnv of
  (Right result) -> do
    tell $ "\nEvaluated to: " ++ show result
    return result
  (Left err) -> do
    tell $ err
    mzero

tryTest :: (String, Expr) -> Maybe String
tryTest (input, expected) =
  case runRepl input of
    (Nothing, log) -> Just log
    (Just (result, _), log) -> if result == expected
      then Nothing
      else Just $ "\n---TEST FAILED---" ++ "\nExpected: " ++ show expected ++ "\nBut got: " ++ show result ++ log 
