import Parsing
import AST

main :: IO ()
main = do
  l <- getLine
  eval l

eval string = case (applyParser parse string) of
  Right parsedExpr -> print $ show parsedExpr
  Left e -> print e
