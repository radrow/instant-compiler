module Instant(parse, toJVM, toLLVM) where

import           Instant.Parse
import qualified Instant.JVM as JVM
import qualified Instant.LLVM as LLVM

import Instant.Types


parse :: String -> String -> Either String Instant
parse filename code = entail <$> parseInstant filename code


toJVM :: String -> Instant -> String
toJVM = JVM.build


toLLVM :: Instant -> String
toLLVM = LLVM.build
