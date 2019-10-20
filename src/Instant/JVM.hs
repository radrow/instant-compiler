module Instant.JVM(build) where

import Control.Monad.Reader
import Instant.Types

import qualified Data.Map as M
import Data.Map(Map)

data JVMOp
  = ICONST Int
  | ISTORE Int
  | ILOAD Int
  | ADD
  | SUB
  | MUL
  | DIV
  | SWAP
  | GETSTATIC String String
  | INVOKEVIRTUAL Int String
  deriving (Show)

type JVM = [JVMOp]

serializeOp :: JVMOp -> String
serializeOp = \case
  ICONST (-1) -> "iconst_m1"
  ICONST n | n <= 5 -> "iconst_" ++ show n
  ICONST n -> "bipush " ++ show n

  ISTORE n | n < 0 -> error $ "Bad store num: " <> show n
  ISTORE n | n <= 3 -> "istore_" ++ show n
  ISTORE n -> "istore " ++ show n

  ILOAD n | n < 0 -> error $ "Bad load num: " <> show n
  ILOAD n | n <= 3 -> "iload_" ++ show n
  ILOAD n -> "iload " ++ show n

  ADD -> "iadd"
  SUB -> "isub"
  MUL -> "imul"
  DIV -> "idiv"

  SWAP -> "swap"

  GETSTATIC t c -> "getstatic " ++ t ++ " " ++ c
  INVOKEVIRTUAL _ m -> "invokevirtual " ++ m


invocation :: String -> String
invocation filename = unlines
  [ ".source " ++ filename
  , ".class public Inst"
  , ".super java/lang/Object"
  , ""
  , ".method public <init>()V"
  , "  aload_0"
  , "  invokenonvirtual java/lang/Object/<init>()V"
  , "  return"
  , ".end method"
  ]


type JVMCompiler = Reader (Map String Int)
getVar :: String -> JVMCompiler Int
getVar v = asks (M.! v)

compileExpr :: Expr -> JVMCompiler JVM
compileExpr = \case
  EInt i -> pure [ICONST i]
  EVar v -> getVar v >>= \i -> pure [ILOAD i]

  EPlus a b -> join <$> sequence
    -- because of assoc requirement
    [ compileExpr b
    , compileExpr a
    , pure [ADD]
    ]
  EMinus a b -> join <$> sequence [compileExpr a, compileExpr b, pure [SUB]]
  EMult a b -> join <$> sequence [compileExpr a, compileExpr b, pure [MUL]]
  EDiv a b -> join <$> sequence [compileExpr a, compileExpr b, pure [DIV]]

compileStmt :: InstantStmt -> JVMCompiler JVM
compileStmt = \case
  IExpr e -> join <$> sequence
    [ pure [GETSTATIC "java/lang/System/out" "Ljava/io/PrintStream;"]
    , compileExpr e
    , pure [INVOKEVIRTUAL 1 "java/io/PrintStream/println(I)V"]
    ]
  IAssg v e -> do
    easm <- compileExpr e
    idx <- getVar v
    pure $ easm ++ [ISTORE idx]


compileInstant :: String -> Instant -> JVMCompiler String
compileInstant filename code = do
  varsCount <- asks M.size
  jvm <- join <$> mapM compileStmt (instantCode code)
  let funBody = unlines . fmap (("  "<>) . serializeOp) $ jvm
  pure $
    invocation filename ++
    unlines
    [ ""
    , ".method public static main([Ljava/lang/String;)V"
    , ".limit locals " ++ show (varsCount + 1)
    , ".limit stack " ++ show (estimateStackSize jvm)
    ] ++
    funBody ++
    "return\n" ++
    ".end method\n"

build :: String -> Instant -> String
build filename code =
  runReader (compileInstant filename code) (varMap code)


estimateStackSize :: JVM -> Int
estimateStackSize = maximum . scanl estimate 0 where
  estimate :: Int -> JVMOp -> Int
  estimate prev = \case
      ICONST _ -> prev + 1
      ISTORE _ -> prev - 1

      ILOAD _ -> prev + 1

      ADD -> prev - 1
      SUB -> prev - 1
      MUL -> prev - 1
      DIV -> prev - 1

      SWAP -> prev

      GETSTATIC _ _ -> prev + 1
      INVOKEVIRTUAL args _ -> prev - args