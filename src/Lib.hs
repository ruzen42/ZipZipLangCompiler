module Lib
    ( compile
    ) where

import System.Exit (exitFailure)
import Lexer (alexScanTokens)
import Parser (parse)
import PrettyPrinter (prettyProgram)
import SemanticAnalyzer (analyzeProgram, printAnalysis, hasErrors)
import AST
import Control.Monad (when)

compile :: Bool -> String -> IO ()
compile verbose filename = do
    content <- readFile filename

    let tokens = alexScanTokens content
    when verbose $ do
        putStrLn "=== TOKENS ==="
        print tokens
        putStrLn ""

    let ast = parse tokens
    when verbose $ do
        putStrLn "=== RAW AST ==="
        print ast
        putStrLn ""

    putStrLn "=== AST (Pretty) ==="
    putStrLn $ prettyProgram ast
    putStrLn ""

    let analysis = analyzeProgram ast
    printAnalysis analysis

    if hasErrors analysis
    then do
        putStrLn "Compilation failed due to errors."
        exitFailure
    else
        putStrLn "✅ Compilation successful!"
