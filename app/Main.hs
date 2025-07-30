module Main (main) where

import Options.Applicative
import Control.Monad (when)
import Compiler 

data Options = Options 
  { srcFiles    :: [String]
  , verboseMode :: Bool 
  , lexing   :: Bool 
  , onlyParse   :: Bool 
  , outputName  :: String 
  } deriving (Show)

main :: IO ()
main = do 
  opts <- execParser optsInfo 
  contents <- readFile (srcFiles opts !! 0)
  putStrLn $ compile contents
  when (lexing opts) $ putStrLn $ printLexer contents


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
