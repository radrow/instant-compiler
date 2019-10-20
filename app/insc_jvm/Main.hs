module Main where

import Instant
import System.Environment
import System.Exit
import System.IO
import System.FilePath


main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> do
      contents <- readFile file
      case parse file contents of
        Left e -> hPutStrLn stderr e >> exitFailure
        Right inst -> do
          let fileName = takeFileName file
              newName = replaceExtension fileName "j"
          writeFile newName (toJVM newName inst)
    _ -> hPutStrLn stderr "BAD ARGS" >> exitFailure
