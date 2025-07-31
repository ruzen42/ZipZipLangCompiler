module Compiler (compile, printLexer) where

import Preprocessor
import Lexer 

compile, printLexer :: String -> String 

compile input = 
    lexingString $ case preprocess of
                      Right -> r

printLexer input = 
  either (("Lexer error: " ++) . show) show $
    lexingString input
