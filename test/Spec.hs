import AST
import Parsing.Combinators hiding ( (<|>) )
import Control.Applicative
import Evaluation.Evaluator
import Repl


main :: IO ()
main  = putStrLn $ runTests $ map tryTest [
  ("1", Constant 1),
  ("1 + 2", Constant 3),

  ("2 + 5", Constant 7),
  ("10 - 5", Constant 5),
  ("5 * 2", Constant 10),
  ("20 / 2", Constant 10),
  ("-1.0", Constant (-1)),

  ("4 * 2 + 4", Constant 12),
  ("4 + 2 * 4", Constant 12),
  ("4 * 2 * 4", Constant 32),
  ("4 + 2 + 4", Constant 10),
  ("5 * 5 * 5 * 5", Constant 625),

  ("5 + 5 + 5 + 5", Constant 20),
  ("0 + 10 - 10", Constant 0),
  ("1 * 10 / 10", Constant 1),
  ("10 + -5", Constant 5),

  ("3 * (2 + 2)", Constant 12),
  ("sin 0", Constant 0),
  ("sin sin sin 0", Constant 0),
  ("3 * sin 0", Constant 0),
  
  ("cos 0", Constant 1),
  ("3 * cos 0", Constant 3),
  --("3 + Answer", Constant 45),
  ("5 = x", Constant 5),
  ("5 = w = y = a = b", Constant 5),
  ("x = x", Identifier "x"),
  ("x + 1 = x", Binary Addition (Identifier "x") (Constant 1)),
  ("{1 = x} + {1 = x}", Constant 2),
  ("{1 = x} + {x}", Binary Addition (Constant 1) (Identifier "x")),
  ("{{1 = x} = x}", Constant 1),
  ("{{2 = x} + {1 = x}}", Constant 3),
  ("(1 = x) + {(2 + x = x) + {3 + x = x}}", Constant 10)
  ]


type Test = Maybe String

runTests :: [Test] -> String
runTests tests = case foldl1 (<|>) tests of
  Just err -> "TEST FAILED: " ++ err
  Nothing -> "All tests passed!"  

