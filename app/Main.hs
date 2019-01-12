import Parsing
import AST

main :: IO ()
main = do
  l <- getLine
  eval l

eval string = case (applyParser parse string) of
  Just parsedExpr -> print $ show parsedExpr
  Nothing -> print "Please insert something to evaluate"
