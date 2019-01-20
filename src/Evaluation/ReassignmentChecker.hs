module Evaluation.ReassignmentChecker where
import AST
import Control.Monad.Reader
import Control.Monad.Identity
import qualified Data.HashMap.Lazy as H

type Environment = H.HashMap String Expr

type Checker = ReaderT Environment (Maybe) String

runChecker :: Checker -> Maybe String
runChecker = flip runReaderT H.empty

check :: Expr -> Checker

check (Binary (Assignment) l (Identifier var)) = do
  env <- ask
  case H.lookup var env of
    Just v -> return "IDENTIFIER REASSIGNED!!"
    Nothing -> lift Nothing
    --Run local?

check (Unary _ arg) = check arg

check (Binary _ l r) = do
  check l
  check r

check (Scope s) = local (\env -> H.empty) $ check s

check _ = do lift Nothing
