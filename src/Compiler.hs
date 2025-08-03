module Compiler (compile, printLexer) where

import Preprocessor
import Lexer 

compile, printLexer :: String -> String 

compile code = 
  either (("Error: " ++) . show) show $
    preprocess code >>= lexingString 

printLexer code = 
  either (("Lexer error: " ++) . show) show $
    lexingString code

--printAST code = 
