import System.IO
import Parsing.Combinators
import AST
import Evaluation.Evaluator
import qualified Data.HashMap.Lazy as H

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  l <- getLine
  if l == "read"
    then do
    file <- getLine
    src <- readFile file
    eval src
    else
    eval l

  
eval string = case runParser parse string of
  Right parsedExpr -> do
    putStrLn $ show parsedExpr
    case runEvaluator emptyEnv $ evaluate parsedExpr of
      Right (result, env) -> do
        putStrLn $ "RESULT: \n" ++ show result
        printEnv env
      Left err -> putStrLn $ "EVAL ERROR: " ++ err
        
  Left e -> print $ "PARSE ERROR: " ++ show e


printEnv :: Environment -> IO ()
printEnv env = do
   putStrLn "VARIABLES:"
   mapM_ (\(key, val) -> putStrLn (key ++ " = " ++ (show val))) (H.toList (fst env))
   putStrLn "FUNCTIONS:"
   mapM_ (\(key, val) -> putStrLn (key ++ " = " ++ (show val))) (H.toList (snd env))
