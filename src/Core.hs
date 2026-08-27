module Core (parseAndRun, dumpProgram, loadProgram) where
 
import Data.Text (Text)
import Data.Vector (Vector)
import VM
import Parse (parse)
import Control.Monad.State (runStateT)
import Data.Binary (decodeFile, encodeFile)
 
parseAndRun :: Text -> IO ()
parseAndRun code =
  case Parse.parse code of
    Right program -> do
      _ <- runStateT (runVM program) initVM
      pure ()
    Left err -> error err

dumpProgram :: FilePath -> Vector Instruction -> IO ()
dumpProgram name program = encodeFile name program

loadProgram :: FilePath -> IO (Vector Instruction)
loadProgram path = decodeFile path
