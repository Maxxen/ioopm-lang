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
    case evaluate parsedExpr of
      Right x -> print $ "RESULT: " ++ show x
      Left ex -> print $ "PARTIAL RESULT: " ++ (show ex)
  Left e -> print e
