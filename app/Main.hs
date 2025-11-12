module Main (main) where

import Lib
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    input <- case args of
        [] -> getContents
        [file] -> readFile file
        _ -> error "Usage: zzc [file]"
    print (someFunc input)
