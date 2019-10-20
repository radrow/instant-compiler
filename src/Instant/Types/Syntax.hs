{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module Instant.Types.Syntax where

import qualified Data.Set as S
import qualified Data.Map as M
import           Data.Map(Map)


data TermType
  = E3 -- plus/minus
  | E2 -- mult/div
  | E1 -- paren/lit
  | St -- statement
  | P  -- program
  deriving (Eq, Show)

data AST (termType :: TermType) where
  ASTPlus  :: AST 'E2 -> AST 'E3 -> AST 'E3
  ASTMinus :: AST 'E2 -> AST 'E3 -> AST 'E3
  AST23    :: AST 'E2            -> AST 'E3

  ASTMult  :: AST 'E1 -> AST 'E2 -> AST 'E2
  ASTDiv   :: AST 'E1 -> AST 'E2 -> AST 'E2
  AST12    :: AST 'E1            -> AST 'E2

  ASTInt   :: Int                -> AST 'E1
  ASTVar   :: String             -> AST 'E1
  ASTParen :: AST 'E3            -> AST 'E1

  ASTExpr  :: AST 'E3            -> AST 'St
  ASTAss   :: String -> AST 'E3  -> AST 'St

  AST      :: [AST 'St]          -> AST 'P
deriving instance Eq (AST t)
deriving instance Show (AST t)

data Expr
  = EPlus Expr Expr
  | EMinus Expr Expr
  | EMult Expr Expr
  | EDiv Expr Expr
  | EInt Int
  | EVar String
  deriving (Eq, Show)

data InstantStmt
  = IExpr Expr
  | IAssg String Expr
  deriving (Eq, Show)

newtype Instant = Instant { instantCode :: [InstantStmt] }
  deriving (Eq, Show)

type family EntailedBy (t :: TermType) :: *
type instance EntailedBy 'E1 = Expr
type instance EntailedBy 'E2 = Expr
type instance EntailedBy 'E3 = Expr
type instance EntailedBy 'St = InstantStmt
type instance EntailedBy 'P  = Instant


entail :: AST t -> EntailedBy t
entail (AST stmts) = Instant $ fmap entail stmts
entail (ASTExpr e) = IExpr (rtolBalance $ entail e)
entail (ASTAss name e) = IAssg name (rtolBalance $ entail e)
entail (ASTMult a b) = EMult (entail a) (entail b)
entail (ASTDiv a b) = EDiv (entail a) (entail b)
entail (AST12 e) = entail e
entail (ASTPlus a b) = EPlus (entail a) (entail b)
entail (ASTMinus a b) = EMinus (entail a) (entail b)
entail (AST23 e) = entail e
entail (ASTInt i) = EInt i
entail (ASTVar n) = EVar n
entail (ASTParen e) = entail e


-- Rebalances tree to be left-assotiative
rtolBalance :: Expr -> Expr
rtolBalance = \case
  -- task requirements enforce right-assoc of +
  (EMinus a (EMinus b c)) -> rtolBalance (EMinus (EMinus a b) c)
  (EPlus a (EMinus b c))  -> rtolBalance (EMinus (EPlus a b) c)
  (EMinus a (EPlus b c))  -> rtolBalance (EPlus (EMinus a b) c)
  (EMult a (EMult b c))   -> rtolBalance (EMult (EMult a b) c)
  (EDiv a (EDiv b c))     -> rtolBalance (EDiv (EDiv a b) c)
  (EMult a (EDiv b c))    -> rtolBalance (EDiv (EMult a b) c)
  (EDiv a (EMult b c))    -> rtolBalance (EMult (EDiv a b) c)

  EPlus a b               -> EPlus (rtolBalance a) (rtolBalance b)
  EMinus a b              -> EMinus (rtolBalance a) (rtolBalance b)
  EMult a b               -> EMult (rtolBalance a) (rtolBalance b)
  EDiv a b                -> EDiv (rtolBalance a) (rtolBalance b)

  other -> other


varMap :: Instant -> Map String Int
varMap code = M.fromList $ zip names [1..] where
  names = S.toList $ foldl inserter S.empty (instantCode code)

  inserter prev (IExpr _) = prev
  inserter prev (IAssg v _) = S.insert v prev
