import System.IO
import Parsing.Combinators
import AST
import Repl
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
  
eval input = case runRepl input of
  (Just (result, env), log) -> do 
    putStrLn $ "RESULT: \n" ++ show result ++ log
    printEnv env
  (Nothing, log) -> putStrLn log

printEnv :: Environment -> IO ()
printEnv env = do
   putStrLn "VARIABLES:"
   mapM_ (\(key, val) -> putStrLn (key ++ " = " ++ (show val))) (H.toList (fst env))
   putStrLn "FUNCTIONS:"
   mapM_ (\(key, val) -> putStrLn (key ++ " = " ++ (show val))) (H.toList (snd env))
