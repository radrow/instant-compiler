module Instant.Interpret where

import qualified Data.Map as M
import Data.Map(Map)
import Control.Applicative
import Control.Monad.RWS

import Instant.Types


type Interpret = RWS () String (Map String Int)

eval :: Instant -> Interpret ()
eval code = void $ traverse evalStmt (instantCode code)

evalStmt :: InstantStmt -> Interpret ()
evalStmt = \case
  IExpr e -> evalExpr e >>= \i -> tell (show i <> "\n")
  IAssg n e -> evalExpr e >>= \i -> modify (M.insert n i)

evalExpr :: Expr -> Interpret Int
evalExpr = \case
  EInt i -> pure i
  EVar v -> gets (M.! v)
  EPlus a b  -> liftA2 (+) (evalExpr a) (evalExpr b)
  EMinus a b -> liftA2 (-) (evalExpr a) (evalExpr b)
  EMult a b  -> liftA2 (*) (evalExpr a) (evalExpr b)
  EDiv a b   -> liftA2 div (evalExpr a) (evalExpr b)

interpret :: Instant -> String
interpret inst = let (_, _, res) = runRWS (eval inst) () (M.empty) in res
