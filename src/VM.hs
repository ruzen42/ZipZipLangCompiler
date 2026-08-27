{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module VM where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Vector (Vector)
import qualified Data.Vector as V
import Control.Monad.State
import Control.Monad (when)
import GHC.Generics (Generic)
import Data.Binary (Binary)
import qualified Data.Binary as BN 

type VarName = Text

data Arg 
  = ArgVar VarName
  | ArgLit Value
  deriving (Show, Eq, Generic)

data Value
  = VNum Int
  | VText Text
  | VLogic Bool
  deriving (Show, Eq, Generic)

data Instruction
  = SetVar VarName Value
  | Add VarName VarName VarName
  | Sub VarName VarName VarName
  | Mul VarName VarName VarName
  | Concat VarName VarName VarName
  | JmpIfZero VarName Int
  | Print Arg
  | Halt
  deriving (Show, Eq, Generic)

data VMState = VMState
  { vars    :: HashMap VarName Value
  , pc      :: Int
  , running :: Bool
  } deriving (Show)

instance Binary Instruction
instance Binary Value
instance Binary Arg

instance BN.Binary a => BN.Binary (V.Vector a) where
    put v = do
        BN.put (V.length v)
        V.mapM_ BN.put v

    get = do
        n <- BN.get
        V.replicateM n BN.get

initVM :: VMState
initVM = VMState
  { vars    = HM.empty
  , pc      = 0
  , running = True
  }
  
readVar :: VarName -> StateT VMState IO Value
readVar name = gets $ \st ->
  case HM.lookup name (vars st) of
    Just v  -> v
    Nothing -> error $ "VM: unbound variable " ++ T.unpack name

writeVar :: VarName -> Value -> StateT VMState IO ()
writeVar name value = modify $ \st ->
  st { vars = HM.insert name value (vars st) }

readNum :: VarName -> StateT VMState IO Int
readNum name = do
  v <- readVar name
  case v of
    VNum n  -> pure n
    VLogic t -> error $ "VM: expected Num in variable "
                        ++ T.unpack name ++ ", got Logic " ++ show t
    VText t -> error $ "VM: expected Num in variable "
                        ++ T.unpack name ++ ", got Text " ++ show t

readText :: VarName -> StateT VMState IO Text
readText name = do
  v <- readVar name
  case v of
    VNum n  -> error $ "VM: expected Text in variable "
                        ++ T.unpack name ++ ", got Num " ++ show n
    VLogic l -> error $ "VM: expected Num in variable "
                        ++ T.unpack name ++ ", got Logic " ++ show l
    VText t -> pure t

step :: Vector Instruction -> StateT VMState IO ()
step program = do
  stateMachine <- get
  let currentPC = pc stateMachine

  if currentPC < 0 || currentPC >= V.length program
    then modify $ \s -> s { running = False }
    else do
      let instr = program V.! currentPC
      modify $ \st -> st { pc = pc st + 1 }
      execute instr

execute :: Instruction -> StateT VMState IO ()
execute (SetVar name value) = writeVar name value

execute (Print arg) = do
  v1 <- case arg of
    ArgVar name -> readVar name
    ArgLit val  -> pure val
  let rendered = formatTo v1
  liftIO $ TIO.putStr rendered
  where
    formatTo :: Value -> Text
    formatTo var =
      case var of
        VText t  -> t
        VLogic l -> if l == True then "Yes" else "No"
        VNum n -> T.pack $ show n


execute (Add dest r1 r2) = do
  v1 <- readNum r1
  v2 <- readNum r2
  writeVar dest (VNum (v1 + v2))

execute (Sub dest r1 r2) = do
  v1 <- readNum r1
  v2 <- readNum r2
  writeVar dest (VNum (v1 - v2))

execute (Mul dest r1 r2) = do
  v1 <- readNum r1
  v2 <- readNum r2
  writeVar dest (VNum (v1 * v2))

execute (Concat dest r1 r2) = do
  v1 <- readText r1
  v2 <- readText r2
  writeVar dest (VText (v1 <> v2))

execute (JmpIfZero name target) = do
  val <- readVar name
  case val of
    VNum 0 -> modify $ \st -> st { pc = target }
    VNum _ -> pure ()
    VText _ -> error $ "VM: JmpIfZero expects Num in variable " ++ T.unpack name
    VLogic True -> pure ()
    VLogic False -> modify $ \st -> st { pc = target }

execute Halt = modify $ \st -> st { running = False }

runVM :: Vector Instruction -> StateT VMState IO ()
runVM program = do
  st <- get
  when (running st) $ do
    step program
    runVM program
