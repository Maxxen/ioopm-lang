module Evaluation.PrettyPrinter (prettyPrint) where
import AST
import Data.List

prettyPrint :: Expr -> String
prettyPrint e = showPretty 0 e

showPretty :: Int -> Expr -> String
showPretty d (Block lines) =
  let nd = d + 1
  in "{\n" ++ intercalate "\n" (map (indent nd . showPretty nd) lines) ++ "\n" ++ (indent d "}\n")
showPretty d (Binary op l r) = (showPretty d l) ++ " " ++ show op ++ " " ++ (showPretty d r)
showPretty d (Unary op argument) = show op ++ "(" ++ (showPretty d argument) ++ ")"
showPretty d (Scope expression) = "{" ++ (showPretty d expression) ++ "}"
showPretty d (Constant float) = show float
showPretty d (Identifier ident) = ident
showPretty d (FunctionCall ident arguments) =
  ident ++ "(" ++ intercalate ", " (map (showPretty d) arguments) ++ ")"
  
showPretty d (FunctionDecl ident params (Block body)) =
  let nd = d + 1
  in "function " ++ ident ++ "(" ++ intercalate ", " params ++ ")" ++
     "{\n" ++ intercalate "\n" (map (indent nd . showPretty nd) body) ++ "\n" ++ indent d "}\n"
    
indent :: Int -> String -> String
indent n str = (concat $ replicate n "  ") ++ str
