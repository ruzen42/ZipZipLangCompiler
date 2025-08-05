{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module CodeGen (generateCode, generateLLVM) where

import qualified LLVM.AST as L
import qualified LLVM.AST.Type as L
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Global as G
import qualified LLVM.AST.CallingConvention as CC
import qualified LLVM.AST.Attribute as A
import qualified LLVM.AST.AddrSpace as AS
import qualified LLVM.AST.IntegerPredicate as IP
import LLVM.AST.Name
import LLVM.AST.Operand
import LLVM.AST.Instruction
import LLVM.AST.Terminator
import LLVM.Context
import LLVM.Module
import LLVM.Pretty

import Control.Monad.State
import Control.Monad.Except
import qualified Data.Map as Map
import Data.String (fromString)
import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Char8 as BS

import AST

data CodeGenState = CodeGenState
  { currentBlock :: Name                   
  , blocks :: Map.Map Name BlockState       
  , blockCount :: Int                     
  , count :: Word                        
  , names :: Map.Map String Int         
  } deriving Show

data BlockState = BlockState
  { blockName :: Name                  
  , stack :: [Named Instruction]      
  , term :: Maybe (Named Terminator) 
  } deriving Show

type CodeGen = State CodeGenState

emptyBlock :: Int -> BlockState
emptyBlock i = BlockState (Name (fromString ("entry" ++ show i))) [] Nothing

emptyCodeGenState :: CodeGenState
emptyCodeGenState = CodeGenState (Name "entry") Map.empty 1 0 Map.empty

fresh :: CodeGen Word
fresh = do
  i <- gets count
  modify $ \s -> s { count = i + 1 }
  return $ i + 1

uniqueName :: String -> CodeGen String
uniqueName nm = do
  i <- gets names
  let i' = case Map.lookup nm i of
             Nothing -> 0
             Just ix -> ix + 1
  modify $ \s -> s { names = Map.insert nm i' i }
  return $ nm ++ show i'

entry :: CodeGen Name
entry = gets currentBlock

addBlock :: String -> CodeGen Name
addBlock bname = do
  bls <- gets blocks
  ix <- gets blockCount
  nms <- gets names
  let new = emptyBlock ix
      (qname, supply) = uniqueName bname nms
  modify $ \s -> s { blocks = Map.insert (Name (fromString qname)) new bls
                   , blockCount = ix + 1
                   , names = supply }
  return (Name (fromString qname))

setBlock :: Name -> CodeGen Name
setBlock bname = do
  modify $ \s -> s { currentBlock = bname }
  return bname

getBlock :: CodeGen Name
getBlock = gets currentBlock

modifyBlock :: BlockState -> CodeGen ()
modifyBlock new = do
  active <- gets currentBlock
  modify $ \s -> s { blocks = Map.insert active new (blocks s) }

current :: CodeGen BlockState
current = do
  c <- gets currentBlock
  blks <- gets blocks
  case Map.lookup c blks of
    Just x -> return x
    Nothing -> error $ "No such block: " ++ show c

instr :: Instruction -> CodeGen Operand
instr ins = do
  n <- fresh
  let ref = UnName n
  blk <- current
  let i = stack blk
  modifyBlock $ blk { stack = i ++ [ref := ins] }
  return $ LocalReference L.i32 ref

terminator :: Named Terminator -> CodeGen (Named Terminator)
terminator trm = do
  blk <- current
  modifyBlock $ blk { term = Just trm }
  return trm

llvmType :: String -> L.Type
llvmType "Num" = L.i32
llvmType "Logic" = L.i1
llvmType "Text" = L.ptr L.i8
llvmType "()" = L.VoidType
llvmType _ = L.i32  

codegenExpr :: Expr -> CodeGen Operand
codegenExpr (ENumber n) = return $ ConstantOperand (C.Int 32 (fromIntegral n))

codegenExpr (EBool True) = return $ ConstantOperand (C.Int 1 1)
codegenExpr (EBool False) = return $ ConstantOperand (C.Int 1 0)

codegenExpr (EString s) = do
  let strConstant = C.Array L.i8 (map (C.Int 8 . fromIntegral . fromEnum) (s ++ "\0"))
  return $ ConstantOperand (C.Null (L.ptr L.i8))

codegenExpr (EIdentifier name) = do
  return $ LocalReference L.i32 (Name (fromString name))

codegenExpr (EBinaryOp "+" left right) = do
  l <- codegenExpr left
  r <- codegenExpr right
  instr $ Add False False l r []

codegenExpr (EBinaryOp "*" left right) = do
  l <- codegenExpr left
  r <- codegenExpr right
  instr $ Mul False False l r []

codegenExpr (EBinaryOp "/" left right) = do
  l <- codegenExpr left
  r <- codegenExpr right
  instr $ SDiv False l r []

codegenExpr (EBinaryOp "==" left right) = do
  l <- codegenExpr left
  r <- codegenExpr right
  instr $ ICmp IP.EQ l r []

codegenExpr (EBinaryOp "&&" left right) = do
  l <- codegenExpr left
  r <- codegenExpr right
  instr $ And l r []

codegenExpr (EBinaryOp op _ _) = 
  error $ "Unsupported binary operator: " ++ op

codegenExpr (EIf cond thenExpr elseExpr) = mdo
  condVal <- codegenExpr cond
  
  thenBlock <- addBlock "then"
  elseBlock <- addBlock "else"
  exitBlock <- addBlock "exit"
  
  terminator $ Do $ CondBr condVal thenBlock elseBlock []
  
  setBlock thenBlock
  thenVal <- codegenExpr thenExpr
  terminator $ Do $ Br exitBlock []
  thenBlock' <- getBlock
  
  setBlock elseBlock
  elseVal <- codegenExpr elseExpr
  terminator $ Do $ Br exitBlock []
  elseBlock' <- getBlock
  
  setBlock exitBlock
  instr $ Phi L.i32 [(thenVal, thenBlock'), (elseVal, elseBlock')] []

codegenExpr (EFunctionCall "print" [arg]) = do
  argVal <- codegenExpr arg
  let printfType = L.FunctionType L.i32 [L.ptr L.i8] True
  let printf = ConstantOperand $ C.GlobalReference (L.ptr printfType) (Name "printf")
  instr $ Call Nothing CC.C [] (Right printf) [(argVal, [])] [] []

codegenExpr (EFunctionCall name args) = do
  argVals <- mapM codegenExpr args
  let funcType = L.FunctionType L.i32 (map (const L.i32) args) False
  let func = ConstantOperand $ C.GlobalReference (L.ptr funcType) (Name (fromString name))
  instr $ Call Nothing CC.C [] (Right func) (map (\x -> (x, [])) argVals) [] []

codegenExpr (EBlock exprs) = do
  case exprs of
    [] -> return $ ConstantOperand (C.Undef L.VoidType)
    [expr] -> codegenExpr expr
    (expr:rest) -> do
      _ <- codegenExpr expr
      codegenExpr (EBlock rest)

codegenExpr _ = error "Unsupported expression in code generation"

codegenFunction :: Definition -> CodeGen L.Global
codegenFunction (DFunctionDef name params returnType body) = do
  let paramTypes = map (llvmType . snd) params
  let retType = llvmType returnType
  
  entry <- addBlock "entry"
  setBlock entry
  
  bodyVal <- codegenExpr body
  
  if returnType == "()"
    then terminator $ Do $ Ret Nothing []
    else terminator $ Do $ Ret (Just bodyVal) []
  
  blks <- gets blocks
  let blockList = map createBasicBlock (Map.elems blks)
  
  return $ G.functionDefaults
    { G.name = Name (fromString name)
    , G.parameters = ([Parameter (llvmType pType) (Name (fromString pName)) [] | (pName, pType) <- params], False)
    , G.returnType = retType
    , G.basicBlocks = blockList
    }

codegenFunction (DMainBlock body) = do
  entry <- addBlock "entry"
  setBlock entry
  
  bodyVal <- codegenExpr body
  
  let exitCode = ConstantOperand (C.Int 32 0)
  terminator $ Do $ Ret (Just exitCode) []
  
  blks <- gets blocks
  let blockList = map createBasicBlock (Map.elems blks)
  
  return $ G.functionDefaults
    { G.name = Name "main"
    , G.parameters = ([], False)
    , G.returnType = L.i32
    , G.basicBlocks = blockList
    }

createBasicBlock :: BlockState -> L.BasicBlock
createBasicBlock (BlockState nm instrs term) = 
  L.BasicBlock nm instrs (case term of
    Just t -> t
    Nothing -> Do $ Ret Nothing [])

codegenModule :: Program -> L.Module
codegenModule (PProgram moduleName definitions) = 
  let globals = evalState (mapM codegenFunction definitions) emptyCodeGenState
      externalDecls = [printfDecl]  
  in L.defaultModule
    { L.moduleName = fromString moduleName
    , L.moduleDefinitions = map L.GlobalDefinition (externalDecls ++ globals)
    }

printfDecl :: L.Global
printfDecl = G.functionDefaults
  { G.name = Name "printf"
  , G.parameters = ([Parameter (L.ptr L.i8) (UnName 0) []], True)
  , G.returnType = L.i32
  , G.linkage = L.External
  }

generateCode :: Program -> L.Module
generateCode = codegenModule

generateLLVM :: Program -> IO String
generateLLVM program = do
  let llvmModule = generateCode program
  return $ show $ pretty llvmModule

