import Parsing.Combinators
import AST
import Evaluation.Evaluator
import qualified Data.HashMap.Lazy as H

main :: IO ()
main = do
  l <- getLine
  eval l

eval string = case (applyParser parse string) of
  Right parsedExpr -> do
    print $ show parsedExpr
    case runEvaluator (evaluate parsedExpr) H.empty of
      Right (result, env) -> do
        print $ "RESULT: " ++ show result
        print "VARIABLES: "
        mapM_ (\(key, val) -> print (key ++ " = " ++ (show val))) (H.toList env) 
        
      Left err -> print $ "EVAL ERROR: " ++ err
  Left e -> print $ "PARSE ERROR: " ++ show e
