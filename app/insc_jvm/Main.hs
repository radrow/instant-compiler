{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Monad
import Control.Exception
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
          jasminName = replaceExtension file "j"
      contents <- readFile file
      case parse file contents >>= toJVM jasminName of
        Left e -> hPutStrLn stderr e >> exitFailure
        Right jasminCode -> do
          writeFile jasminName jasminCode
          when (not noBin) $ do
            let outpath = takeDirectory file
            -- yeah, that's dirty
            catch (callCasual jasminName outpath) $ \(_ :: SomeException) ->
              callRetarded jasminName outpath
    _ -> hPutStrLn stderr "BAD ARGS" >> exitFailure


callCasual :: FilePath -> FilePath -> IO ()
callCasual jasminName outpath = callProcess "jasmin" [jasminName, "-d", outpath]


-- MIMUW requirement
callRetarded :: FilePath -> FilePath -> IO ()
callRetarded jasminName outpath =
  callProcess "java" [ "-jar", "/home/students/inf/PUBLIC/MRJP/Jasmin/jasmin.jar"
                     , jasminName, "-d", outpath]
