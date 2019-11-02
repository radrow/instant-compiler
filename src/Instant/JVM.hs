module Instant.JVM(build) where

import           Control.Monad.Reader
import           Control.Monad.State.Strict
import           Control.Monad.Except
import qualified Data.Map as M
import           Data.Map(Map)
import qualified Data.Set as S
import           Data.Set(Set)
import           System.FilePath

import Instant.Syntax


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
  , ".class public " ++ takeBaseName filename
  , ".super java/lang/Object"
  , ""
  , ".method public <init>()V"
  , "  aload_0"
  , "  invokenonvirtual java/lang/Object/<init>()V"
  , "  return"
  , ".end method"
  ]


type JVMCompiler = ExceptT String (StateT (Set String) (Reader (Map String Int)))


getVarName :: String -> JVMCompiler Int
getVarName v = asks (M.! v)


checkVarInit :: Ann -> String -> JVMCompiler ()
checkVarInit ann v =
  get >>= \s -> if S.member v s
                then pure ()
                else throwError $ at ann ++ " Undefined variable " ++ v


registerVar :: String -> JVMCompiler ()
registerVar v = modify (S.insert v)


comutative :: JVMOp -> Bool
comutative ADD = True
comutative MUL = True
comutative _   = False


compileExpr :: Expr -> JVMCompiler JVM
compileExpr e = ($[]) . snd <$> builder e where
  builder :: Expr -> JVMCompiler (Int, JVM -> JVM)
  builder = \case -- optimizes stack
    EInt _ i     -> pure (1, ((ICONST i):))
    EVar ann v   -> do
      checkVarInit ann v
      i <- getVarName v
      pure (1, ((ILOAD i):))
    EPlus _ a b  -> buildOp ADD a b
    EMinus _ a b -> buildOp SUB a b
    EMult _ a b  -> buildOp MUL a b
    EDiv _ a b   -> buildOp DIV a b

  buildOp op a b = do
      (ai, ab) <- builder a
      (bi, bb) <- builder b
      case compare ai bi of
        EQ -> pure (ai + 1, ab . bb . (op:))
        LT -> pure (bi    , bb . ab . (if comutative op then id else (SWAP:)) . (op:))
        GT -> pure (ai    , ab . bb . (op:))


compileStmt :: InstantStmt -> JVMCompiler JVM
compileStmt = \case
  IExpr _ e -> join <$> sequence
    [ pure [GETSTATIC "java/lang/System/out" "Ljava/io/PrintStream;"]
    , compileExpr e
    , pure [INVOKEVIRTUAL 1 "java/io/PrintStream/println(I)V"]
    ]
  IAssg _ v e -> do
    easm <- compileExpr e
    registerVar v
    idx <- getVarName v
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


build :: String -> Instant -> Either String String
build filename code =
  runReader (evalStateT (runExceptT $ compileInstant filename code) S.empty) (varMap code)


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
