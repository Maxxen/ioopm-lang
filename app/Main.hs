import Parsing
import AST
import Evaluation

main :: IO ()
main = do
  l <- getLine
  eval l

eval string = case (applyParser parse string) of
  Right parsedExpr -> do
    print $ show parsedExpr
    case runEvaluator (evalStrict parsedExpr) emptyEnv of
      Right x -> print $ "RESULT: " ++ show x
      Left err -> print $ "EVAL ERROR: " ++ err
  Left e -> print $ "PARSE ERROR: " ++ e
