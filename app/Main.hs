module Main (main) where

import Options.Applicative
import Lexer 
import Text.Parsec 

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
  let filepath :: FilePath 
      filepath = srcFiles opts !! 0                        
  contents <- readFile filepath 
  if lexing opts then 
    print $ lexing contents
  else return ()

FileLexing :: String -> String
FileLexing contents = do
    case parse lexer "main" contents of
      Left err -> show err
      Right ts -> show ts

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
        <> value "a.out"
        <> help "Name of the output executable"
        )

optsInfo = info (optionsParser <**> helper)
  ( fullDesc
  <> progDesc "Compile your zzlang files"
  <> header "zzc - The ZZ Language Compiler"
  )
