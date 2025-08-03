module OutputBindings (printErr, printWarn) where

import System.Console.ANSI

printErr :: String -> IO ()
printErr message = bind message "Error: " Red 

printWarn :: String -> IO ()
printWarn message = bind message "Warning: " Yellow 

bind :: String -> String -> Color -> IO ()
bind message outputType color = do 
  setSGR [SetColor Foreground Vivid color] 
  putStr outputType 
  setSGR [Reset] 
  putStrLn message 
