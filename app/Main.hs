module Main (main) where

import Options.Applicative
import Control.Monad (when)
import Compiler 
import Control.Exception
import System.Console.ANSI

data Options = Options 
  { srcFiles    :: [String]
  , verboseMode :: Bool 
  , lexing   :: Bool 
  , onlyParse   :: Bool 
  , outputName  :: String 
  } deriving (Show)

printErr :: String -> IO ()
printErr s = do 
  setSGR [SetColor Foreground Vivid Red] 
  putStr "Error: "
  setSGR [Reset] 
  putStrLn s

main :: IO ()
main = do 
  opts <- execParser optsInfo 
  let fileName = srcFiles opts !! 0
  result <- try (readFile fileName) :: IO (Either IOException String)
  case result of
    Left ex -> printErr $ show ex 
    Right contents -> do 
      when (lexing opts) $ putStrLn $ printLexer contents
      putStrLn $ compile contents

optsInfo :: ParserInfo Options

optionsParser :: Parser Options
optionsParser = Options
  <$> some (argument str (metavar "SRC_FILES..." <> help "Source files to compile"))
  <*> switch (long "verbose" <> short 'v' <> help "Enable verbose mode")
  <*> switch (long "lexer" <> short 'l' <> help "Print lexing files")
  <*> switch (long "parse" <> short 'p' <> help "Parsing files")
  <*> strOption
        ( long "output"
        <> short 'o'
        <> metavar "OUTPUT_NAME"
        <> value "Main"
        <> help "Name of the output executable"
        )

optsInfo = info (optionsParser <**> helper)
  ( fullDesc
  <> progDesc "ZZlang compiler"
  <> header "zzc - The ZipZipLang Compiler"
  )
