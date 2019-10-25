module Instant.Interpret where

import qualified Data.Map as M
import           Data.Map(Map)
import           Control.Applicative
import           Control.Monad.RWS.Strict
import           Control.Monad.Except

import Instant.Syntax


type Interpret = RWST () String (Map String Int) (Except String)


eval :: Instant -> Interpret ()
eval code = void $ traverse evalStmt (instantCode code)


evalStmt :: InstantStmt -> Interpret ()
evalStmt = \case
  IExpr _ e -> evalExpr e >>= \i -> tell (show i <> "\n")
  IAssg _ n e -> evalExpr e >>= \i -> modify (M.insert n i)


evalExpr :: Expr -> Interpret Int
evalExpr = \case
  EInt _ i -> pure i
  EVar ann v -> gets (M.lookup v) >>=
    maybe (throwError $ at ann ++ " No such variable " ++ v) pure
  EPlus _ a b  -> liftA2 (+) (evalExpr a) (evalExpr b)
  EMinus _ a b -> liftA2 (-) (evalExpr a) (evalExpr b)
  EMult _ a b  -> liftA2 (*) (evalExpr a) (evalExpr b)
  EDiv _ a b   -> liftA2 div (evalExpr a) (evalExpr b)


interpret :: Instant -> Either String String
interpret inst =
  runExcept (fmap (\(_, _, res) -> res) (runRWST (eval inst) () M.empty))
