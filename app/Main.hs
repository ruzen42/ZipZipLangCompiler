module Main (main) where

import Lib
import System.Environment (getArgs)
import Options.Applicative

data Options = Options
    { files      :: ![Filename]
    , verbose    :: !Bool
    , outputFile :: !Filename
    }

main :: IO ()
main = do
    Options{..} <- execParser opts
    input <- case files of
        [] -> getContents
        [file] -> readFile file
        _ -> error "Usage: zzc [file]"
    print (someFunc input)

optionsParser :: Parser Options
optionsParser = Options
    <$> some (strArgument
        ( metavar "FILES..."
       <> help "Input files" ))
    <*> switch
        ( long "verbose"
       <> short 'v'
       <> help "Enable verbose mode" )
    <*> strOption
        ( long "output"
       <> short 'o'
       <> metavar "OUTPUT"
       <> help "Output file" )
