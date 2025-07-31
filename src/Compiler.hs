module Compiler (compile, printLexer) where

import Preprocessor
import Lexer 

compile, printLexer :: String -> String 

compile input = 
  either (("Error: " ++) . show) show $
    preprocess input >>= lexingString 

printLexer input = 
  either (("Lexer error: " ++) . show) show $
    lexingString input
