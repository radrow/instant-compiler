module Instant.PP where

import Instant.Syntax


ppExpr :: Expr -> String
ppExpr = \case
  EInt _ i -> show i
  EVar _ s -> s
  EPlus _ a b -> "(" <> ppExpr a <> " + " <> ppExpr b <> ")"
  EMinus _ a b -> "(" <> ppExpr a <> " - " <> ppExpr b <> ")"
  EMult _ a b -> "(" <> ppExpr a <> " * " <> ppExpr b <> ")"
  EDiv _ a b -> "(" <> ppExpr a <> " / " <> ppExpr b <> ")"


ppStmt :: InstantStmt -> String
ppStmt = \case
  IExpr _ e -> ppExpr e
  IAssg _ v e -> v <> " = " <> ppExpr e


ppInstant :: Instant -> String
ppInstant = foldMap ((<>"\n") . ppStmt) . instantCode
