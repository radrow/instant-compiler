{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module Instant.Types.Syntax where

import qualified Data.Set as S
import qualified Data.Map as M
import           Data.Map(Map)


data TermType
  = E1 -- plus
  | E2 -- minus
  | E3 -- mult/div
  | E4 -- paren/lit
  | St -- statement
  | P  -- program
  deriving (Eq, Show)

data AST (termType :: TermType) where
  ASTPlus  :: AST 'E2 -> AST 'E1 -> AST 'E1
  AST21    :: AST 'E2            -> AST 'E1

  ASTMinus :: AST 'E2 -> AST 'E3 -> AST 'E2
  AST32    :: AST 'E3            -> AST 'E2

  ASTMult  :: AST 'E3 -> AST 'E4 -> AST 'E3
  ASTDiv   :: AST 'E3 -> AST 'E4 -> AST 'E3
  AST43    :: AST 'E4            -> AST 'E3

  ASTInt   :: Int                -> AST 'E4
  ASTVar   :: String             -> AST 'E4
  ASTParen :: AST 'E1            -> AST 'E4

  ASTExpr  :: AST 'E1            -> AST 'St
  ASTAss   :: String -> AST 'E1  -> AST 'St

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
type instance EntailedBy 'E4 = Expr
type instance EntailedBy 'St = InstantStmt
type instance EntailedBy 'P  = Instant


entail :: AST t -> EntailedBy t
entail (AST stmts) = Instant (fmap entail stmts)
entail (ASTExpr e) = IExpr (entail e)
entail (ASTAss name e) = IAssg name (entail e)
entail (ASTPlus a b) = EPlus (entail a) (entail b)
entail (AST21 e) = entail e
entail (ASTMinus a b) = EMinus (entail a) (entail b)
entail (AST32 e) = entail e
entail (ASTMult a b) = EMult (entail a) (entail b)
entail (ASTDiv a b) = EDiv (entail a) (entail b)
entail (AST43 e) = entail e
entail (ASTInt i) = EInt i
entail (ASTVar n) = EVar n
entail (ASTParen e) = entail e


varMap :: Instant -> Map String Int
varMap code = M.fromList $ zip names [1..] where
  names = S.toList $ foldl inserter S.empty (instantCode code)

  inserter prev (IExpr _) = prev
  inserter prev (IAssg v _) = S.insert v prev
