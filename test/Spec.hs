import AST
import Parsing.Combinators
import Control.Applicative

main :: IO ()
main = putStrLn $ runTests [test1, test2, test3]


type Test = Maybe String

runTests :: [Test] -> String
runTests tests = case foldl1 (<|>) tests of
  Just err -> "TEST FAILED: " ++ err
  Nothing -> "All tests passed!"


  
test1 = Nothing
test2 = createParsingTest "1" (Constant 1)
test3 = createParsingTest "1 + 2" (Binary Addition (Constant 1) (Constant 2))



createParsingTest :: String -> Expr -> Maybe String
createParsingTest input expected =
  case applyParser parse input of
    Right expr -> case expr == expected of
      True -> Nothing
      False -> Just $ "Error, expected: " ++ (show expected) ++ ", but got: " ++ (show expr) 
    Left err -> Just $ show err
