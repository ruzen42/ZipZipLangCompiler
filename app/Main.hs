module Main (main) where

import Options.Applicative
import Control.Monad (when, forM_)
import Compiler 
import Control.Exception
import OutputBindings
import Compiler ()

data Options = Options 
  { srcFiles         :: [String]
  , verboseMode      :: Bool 
  , lexing           :: Bool 
  , onlyParse        :: Bool 
  , semanticAnalysis :: Bool 
  , outputName       :: String 
  } deriving (Show)

main :: IO ()
main = do 
  printWarn "This program in development, bugs everywhere"
  opts <- execParser optsInfo 
  forM_ (srcFiles opts) $ \fileName -> do
    result <- try (readFile fileName) :: IO (Either IOException String)
    case result of
      Left ex -> printErr $ show ex 
      Right contents -> do 
        when (lexing opts) $ putStrLn $ printLexer contents 
        when (onlyParse opts) $ putStrLn $ printAST contents 
        when (semanticAnalysis opts) $ putStrLn $ printAST contents
        when (verboseMode opts) $ compileSteps contents 

optsInfo :: ParserInfo Options

optionsParser :: Parser Options
optionsParser = Options
  <$> some (argument str (metavar "SRC_FILES..." <> help "Source files to compile"))
  <*> switch (long "verbose" <> short 'v' <> help "Enable verbose mode")
  <*> switch (long "lexer" <> short 'l' <> help "Print lexing files")
  <*> switch (long "parse" <> short 'p' <> help "Parsing files")
  <*> switch (long "semantic-analys" <> help "Step by step")
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
