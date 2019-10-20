module Instant.LLVM(build) where

import           Data.List
import           Data.Map(Map)
import qualified Data.Map as M
import           Control.Monad.State

import Instant.Types


data LLVMType
  = LLTInt Int
  | LLTPtr LLVMType
  | LLTVargs


i32 :: LLVMType
i32 = LLTInt 32


i8 :: LLVMType
i8 = LLTInt 8


ptr :: LLVMType -> LLVMType
ptr = LLTPtr


data LLVMLit
  = LLLReg String
  | LLLInt Int
  | LLLGetElementPtr (Int, LLVMType) String (LLVMType, LLVMLit) (LLVMType, LLVMLit)


data LLVMOp
  = LLOAssg String LLVMExpr
  | LLOCall LLVMType [LLVMType] String [(LLVMType, LLVMLit)]
  | LLORet LLVMType LLVMLit


data LLVMExpr
  = LLEAdd LLVMType LLVMLit LLVMLit
  | LLESub LLVMType LLVMLit LLVMLit
  | LLEMul LLVMType LLVMLit LLVMLit
  | LLEDiv LLVMType LLVMLit LLVMLit


type LLVM = [LLVMOp]


serializeLit :: LLVMLit -> String
serializeLit = \case
  LLLReg s -> "%" <> s
  LLLInt i -> show i
  LLLGetElementPtr (x, y) ref (t1, v1) (t2, v2) ->
    concat [ "getelementptr inbounds ("
           , "[", show x, " x ", serializeType y, "], "
           , "[", show x, " x ", serializeType y, "]* ", ref, ", "
           , serializeType t1, " ", serializeLit v1, ", "
           , serializeType t2, " ", serializeLit v2, ")"
           ]


serializeType :: LLVMType -> String
serializeType = \case
  LLTInt i -> "i" <> show i
  LLTPtr t -> serializeType t <> "*"
  LLTVargs -> "..."


serializeExpr :: LLVMExpr -> String
serializeExpr = \case
  LLEAdd t a b -> "add " <> serializeType t <> " "
                  <> serializeLit a <> ", "<> serializeLit b
  LLESub t a b -> "sub " <> serializeType t <> " "
                  <> serializeLit a <> ", "<> serializeLit b
  LLEMul t a b -> "mul " <> serializeType t <> " "
                  <> serializeLit a <> ", "<> serializeLit b
  LLEDiv t a b -> "sdiv " <> serializeType t <> " "
                  <> serializeLit a <> ", "<> serializeLit b


serializeOp :: LLVMOp -> String
serializeOp = \case
  LLOAssg s e -> "%" <> s <> " = " <> serializeExpr e
  LLOCall rett argst fun args -> concat
    [ "call "
    , serializeType rett, " "
    , bracSep (fmap serializeType argst) <> " "
    , fun
    , bracSep (fmap (\(t, l) -> serializeType t <> " " <> serializeLit l) args)
    ]
  LLORet t v -> "ret " <> serializeType t <> " " <> serializeLit v


bracSep :: [String] -> String
bracSep elems = '(' : concat (intersperse ", " elems) ++ ")"


data CompilerState = CompilerState
  { csStore :: Int
  , csVarMap :: Map String Int
  }


type LLVMCompiler = State CompilerState


lastId :: LLVMCompiler Int
lastId = gets csStore


newRef :: LLVMCompiler String
newRef = modify (\s -> s{csStore = csStore s + 1}) *> (("val_"<>) . show <$> lastId)


lookupVar :: String -> LLVMCompiler String
lookupVar v = gets ((M.! v) . csVarMap) >>= \i -> pure ("var_" ++ v ++ show i)


initVar :: String -> LLVMCompiler String
initVar v = gets ((M.lookup v) . csVarMap) >>= \case
  Nothing ->
    modify (\s -> s{csVarMap = M.insert v 0 (csVarMap s)}) >> initVar v
  Just i ->
    modify (\s -> s{csVarMap = M.insert v (i + 1) (csVarMap s)}) >> lookupVar v


compileOperator :: (LLVMType -> LLVMLit -> LLVMLit -> LLVMExpr)
                -> LLVMType -> Expr -> Expr
                -> LLVMCompiler (LLVMLit, LLVM)
compileOperator op t a b = do
  (aref, acode) <- compileExpr a
  (bref, bcode) <- compileExpr b
  i <- newRef
  let icode = [LLOAssg i (op t aref bref)]
  pure (LLLReg i, acode ++ bcode ++ icode)


compileExpr :: Expr -> LLVMCompiler (LLVMLit, LLVM)
compileExpr = \case
  EInt i -> pure (LLLInt i, [])
  EVar s -> do
    v <- lookupVar s
    pure (LLLReg v, [])
  EPlus a b -> compileOperator LLEAdd i32 a b
  EMinus a b -> compileOperator LLESub i32 a b
  EMult a b -> compileOperator LLEMul i32 a b
  EDiv a b -> compileOperator LLEDiv i32 a b


compileStmt :: InstantStmt -> LLVMCompiler LLVM
compileStmt = \case
  IAssg v e -> do
    (valRef, ecode) <- compileExpr e
    ref <- initVar v
    pure $ ecode ++ [LLOAssg ref (LLEAdd i32 valRef (LLLInt 0))]
  IExpr e -> do
    (valRef, ecode) <- compileExpr e
    let call =
          LLOCall i32 [ptr i8, LLTVargs] "@printf"
          [ ( ptr i8
            , LLLGetElementPtr (4, i8) "@.intprint" (i32, LLLInt 0) (i32, LLLInt 0)
            )
          , (i32, valRef)
          ]
    pure $ ecode ++ [call]


invocation :: String
invocation = unlines
  [ "@.intprint = private unnamed_addr constant [4 x i8] c\"%d\\0A\\00\", align 1"
  , "declare dso_local i32 @printf(i8*, ...) #1"
  , ""
  , "define dso_local i32 @main() {\n"
  ]


compileInstant :: Instant -> LLVMCompiler String
compileInstant code = do
  llcode <- (++[LLORet i32 (LLLInt 0)]) . join <$>
    traverse compileStmt (instantCode code)
  pure $ invocation ++ concat (fmap ((<>"\n") . ("  "<>) . serializeOp) llcode) ++ "\n}"


build :: Instant -> String
build code =
  evalState (compileInstant code) (CompilerState 0 M.empty)

