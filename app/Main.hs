module Main (main) where

import Options.Applicative

data Options = Options 
  { srcFiles    :: [String]
  , verboseMode :: Bool 
  , onlyLexer   :: Bool 
  , onlyParse   :: Bool 
  , outputName  :: String 
  } deriving (Show)

optionsParser :: Parser Options
optionsParser = Options
  <$> some (argument str (metavar "SRC_FILES..." <> help "Source files to compile"))
  <*> switch (long "verbose" <> short 'v' <> help "Enable verbose mode")
  <*> switch (long "only-lexer" <> short 'l' <> help "Lexing files")
  <*> switch (long "only-parse" <> short 'p' <> help "Parsing files")
  <*> strOption
        ( long "output"
        <> short 'o'
        <> metavar "OUTPUT_NAME"
        <> value "a.out"
        <> help "Name of the output executable"
        )

main :: IO ()
main = do 
  opts <- execParser optsInfo 
  print opts

optsInfo :: ParserInfo Options
optsInfo = info (optionsParser <**> helper)
  ( fullDesc
  <> progDesc "Compile your zzlang files"
  <> header "zzc - The ZZ Language Compiler"
  )
