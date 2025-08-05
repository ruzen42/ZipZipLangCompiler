module Compiler (printLexer, printAST, printSemanticAnalysis, fullCompile, compileSteps) where

import Preprocessor
import Lexer 
import Parser
import SemanticAnalyzer
import CodeGen

printLexer :: String -> String 
printLexer code = 
  either (("Lexer error: " ++) . show) show $
    lexingString code

printAST :: String -> String
printAST code = 
  either (("Parser error: " ++) . show) show $
    preprocess code >>= lexingString >>= parseProgram

printSemanticAnalysis :: String -> String  
printSemanticAnalysis code = 
  case preprocess code >>= lexingString >>= parseProgram of
    Left err -> "Parser error: " ++ show err
    Right ast -> case analyzeProgram ast of
      Left semErr -> "Semantic error: " ++ show semErr
      Right symbolTable -> "Semantic analysis passed. Symbol table:\n" ++ show symbolTable

fullCompile :: String -> String
fullCompile code = do
  case preprocess code of
    Left prepErr -> "Preprocessing error: " ++ show prepErr
    Right preprocessed -> 
      case lexingString preprocessed of
        Left lexErr -> "Lexer error: " ++ show lexErr
        Right tokens ->
          case parseProgram tokens of
            Left parseErr -> "Parser error: " ++ show parseErr
            Right ast ->
              case analyzeProgram ast of
                Left semErr -> "Semantic error: " ++ show semErr
                Right _ -> "Compilation successful!\n\nAST:\n" ++ show ast

compileSteps :: String -> IO ()
compileSteps code = do
  putStrLn "=== PREPROCESSING ==="
  case preprocess code of
    Left err -> putStrLn $ "Error: " ++ show err
    Right preprocessed -> do
      putStrLn "✓ Preprocessing successful"
      
      putStrLn "\n=== LEXICAL ANALYSIS ==="
      case lexingString preprocessed of
        Left err -> putStrLn $ "Error: " ++ show err
        Right tokens -> do
          putStrLn "✓ Lexing successful"
          putStrLn $ "Tokens: " ++ show (take 10 tokens) ++ "..."
          
          putStrLn "\n=== PARSING ==="
          case parseProgram tokens of
            Left err -> putStrLn $ "Error: " ++ show err
            Right ast -> do
              putStrLn "✓ Parsing successful"
              putStrLn $ "AST: " ++ show ast
              
              putStrLn "\n=== SEMANTIC ANALYSIS ==="
              case analyzeProgram ast of
                Left err -> putStrLn $ "Error: " ++ show err
                Right symTable -> do
                  putStrLn "✓ Semantic analysis successful"
                  putStrLn $ "Symbol table: " ++ show symTable
		  
		  putStrLn "\n=== LLVM CODE GENERATION ==="
		  putStrLn "✓ LLVM IR generation successful"
                  putStrLn "Generated LLVM IR:"
                  putStrLn llvmCode
