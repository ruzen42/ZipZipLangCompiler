module Main (main) where

import Lib (compile)
import Options.Applicative
import Control.Monad (when)
import System.Exit (exitSuccess)

data Options = Options
    { files      :: [String]
    , verbose    :: Bool
    , version    :: Bool
    , outputFile :: String
    } deriving (Show)

ver :: String
ver = "0.1.0.0"

main :: IO ()
main = do
    options <- execParser opts
    let fileList = files options
        verboseMode = verbose options
        showVersion = version options

    when showVersion $ putStrLn ver >> exitSuccess

    mapM_ (compile verboseMode) fileList

    when verboseMode $ print options


opts :: ParserInfo Options
opts = info (helper <*> optionsParser)
    ( fullDesc
    <> progDesc "Compiler for .zzl files"
    <> header "zzc - crossplatform, high-performance zip zip lang compiler" )

optionsParser :: Parser Options
optionsParser = Options
    <$> some (strArgument
        ( metavar "FILES..."
       <> help "Input files" ))
    <*> switch
        ( long "verbose"
       <> short 'V'
       <> help "Enable verbose mode" )
    <*> switch
        ( long "version"
       <> short 'v'
       <> help "Show program version" )
    <*> strOption
        ( long "output"
       <> short 'o'
       <> metavar "OUTPUT"
       <> help "Output file"
       <> value "Main" )
