module Main (main) where

import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Control.Exception (SomeException, try)
import Control.Monad.State (runStateT)
import qualified Data.Text.IO as TIO
import Data.Vector (Vector)
import qualified Data.Vector as V

import VM (Instruction, initVM, runVM)
import Parse (parse)
import Core (parseAndRun, dumpProgram, loadProgram)

main :: IO ()
main = do
  rawArgs <- getArgs
  let args = V.fromList rawArgs
  if V.length args < 2
    then usage
    else do
      let cmd  = args V.! 0
          file = args V.! 1
          rest = V.drop 2 args
      case cmd of
        "compile" -> compile file rest
        "run"     -> run file
        _         -> usage

usage :: IO ()
usage = do
  hPutStrLn stderr "usage: zzc <compile|run> [<file> ...]"
  exitFailure

compile :: FilePath -> Vector FilePath -> IO ()
compile inFile rest = do
  source <- TIO.readFile inFile
  case parse source of
    Left err -> do
      hPutStrLn stderr ("parse error: " ++ err)
      exitFailure
    Right program -> do
      let outFile = if V.null rest then inFile ++ ".zvm" else rest V.! 0
      dumpProgram outFile program

run :: FilePath -> IO ()
run file = do
  loaded <- try (loadProgram file) :: IO (Either SomeException (Vector Instruction))
  case loaded of
    Right program -> do
      _ <- runStateT (runVM program) initVM
      pure ()
    Left _ -> do
      source <- TIO.readFile file
      parseAndRun source
