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
      case parse file contents of
        Left e -> hPutStrLn stderr e >> exitFailure
        Right inst -> do
          let jasminName = replaceExtension file "j"
          writeFile jasminName (toJVM jasminName inst)
          when (not noBin) $ do
            let outpath = takeDirectory file
            callProcess "jasmin" [jasminName, "-d", outpath]
    _ -> hPutStrLn stderr "BAD ARGS" >> exitFailure
