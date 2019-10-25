module Instant(parse, toJVM, toLLVM) where

import           Instant.Parse
import qualified Instant.JVM as JVM
import qualified Instant.LLVM as LLVM

import Instant.Syntax


parse :: String -> String -> Either String Instant
parse filename code = entail <$> parseInstant filename code


toJVM :: String -> Instant -> Either String String
toJVM = JVM.build


toLLVM :: Instant -> Either String String
toLLVM = LLVM.build
