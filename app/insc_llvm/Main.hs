module Main where

import Control.Monad
import System.Environment
import System.Exit
import System.IO
import System.FilePath
import System.Process

import Instant

main :: IO ()
main = do
  args <- getArgs
  case args of
    file:rest -> do
      let noBin = "no-bin" `elem` rest
      contents <- readFile file
      case parse file contents >>= toLLVM of
        Left e -> hPutStrLn stderr e >> exitFailure
        Right llvmCode -> do
          let llFile = replaceExtension file "ll"
          writeFile llFile llvmCode
          when (not noBin) $ do
            let outpath = replaceExtension file "bc"
            callProcess "llvm-as" [llFile, "-o", outpath]
    _ -> hPutStrLn stderr "BAD ARGS: " >> exitFailure
