module Instant.PP where

import Instant.Types.Syntax


ppExpr :: Expr -> String
ppExpr = \case
  EInt i -> show i
  EVar s -> s
  EPlus a b -> "(" <> ppExpr a <> " + " <> ppExpr b <> ")"
  EMinus a b -> "(" <> ppExpr a <> " - " <> ppExpr b <> ")"
  EMult a b -> "(" <> ppExpr a <> " * " <> ppExpr b <> ")"
  EDiv a b -> "(" <> ppExpr a <> " / " <> ppExpr b <> ")"


ppStmt :: InstantStmt -> String
ppStmt = \case
  IExpr e -> ppExpr e
  IAssg v e -> v <> " = " <> ppExpr e


ppInstant :: Instant -> String
ppInstant = foldMap ((<>"\n") . ppStmt) . instantCode
