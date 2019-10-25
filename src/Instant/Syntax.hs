{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module Instant.Syntax where

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

data Ann = Ann { line :: Int, column :: Int, file :: String }
  deriving (Eq, Show)


at :: Ann -> String
at ann =
  file ann ++ ":" ++ show (line ann) ++ ":" ++ show (column ann)


data AST (termType :: TermType) where
  ASTPlus  :: Ann -> AST 'E2 -> AST 'E1 -> AST 'E1
  AST21    ::        AST 'E2            -> AST 'E1

  ASTMinus :: Ann -> AST 'E2 -> AST 'E3 -> AST 'E2
  AST32    ::        AST 'E3            -> AST 'E2

  ASTMult  :: Ann -> AST 'E3 -> AST 'E4 -> AST 'E3
  ASTDiv   :: Ann -> AST 'E3 -> AST 'E4 -> AST 'E3
  AST43    ::        AST 'E4            -> AST 'E3

  ASTInt   :: Ann -> Int                -> AST 'E4
  ASTVar   :: Ann -> String             -> AST 'E4
  ASTParen ::        AST 'E1            -> AST 'E4

  ASTExpr  :: Ann -> AST 'E1            -> AST 'St
  ASTAss   :: Ann -> String -> AST 'E1  -> AST 'St

  AST      ::        [AST 'St]          -> AST 'P
deriving instance Eq (AST t)
deriving instance Show (AST t)

data Expr
  = EPlus Ann Expr Expr
  | EMinus Ann Expr Expr
  | EMult Ann Expr Expr
  | EDiv Ann Expr Expr
  | EInt Ann Int
  | EVar Ann String
  deriving (Eq, Show)

data InstantStmt
  = IExpr Ann Expr
  | IAssg Ann String Expr
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
entail (ASTExpr ann e) = IExpr ann (entail e)
entail (ASTAss ann name e) = IAssg ann name (entail e)
entail (ASTPlus ann a b) = EPlus ann (entail a) (entail b)
entail (AST21 e) = entail e
entail (ASTMinus ann a b) = EMinus ann (entail a) (entail b)
entail (AST32 e) = entail e
entail (ASTMult ann a b) = EMult ann (entail a) (entail b)
entail (ASTDiv ann a b) = EDiv ann (entail a) (entail b)
entail (AST43 e) = entail e
entail (ASTInt ann i) = EInt ann i
entail (ASTVar ann n) = EVar ann n
entail (ASTParen e) = entail e


varMap :: Instant -> Map String Int
varMap code = M.fromList $ zip names [1..] where
  names = S.toList $ foldl inserter S.empty (instantCode code)

  inserter prev (IExpr _ _) = prev
  inserter prev (IAssg _ v _) = S.insert v prev
