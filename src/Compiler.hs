module Compiler (compile, printLexer) where

import Preprocessor
import Lexer 
import Text.Parsec (parse)

compile :: String -> String 
compile input = 
  case preprocess input of
    Left err -> "Preprocessor error: " ++ show err
    Right preprocessed -> 
      case parse lexer "lexer" preprocessed of
        Left err -> "Lexer error: " ++ show err  
        Right tokens -> show tokens

printLexer :: String -> String 
printLexer input = 
  case parse lexer "lexer" input of
    Left err -> "Lexer error: " ++ show err  
    Right tokens -> show tokens
