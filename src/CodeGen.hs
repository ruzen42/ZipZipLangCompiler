module CodeGen where

import AST
import qualified LLVM.AST as L
import qualified LLVM.AST.Type as LT
import qualified LLVM.AST.Constant as LC
import qualified LLVM.AST.Float as LF
import qualified LLVM.AST.IntegerPredicate as LIP
import qualified LLVM.AST.Global as LG
import qualified LLVM.AST.CallingConvention as LCC
import LLVM.AST.Instruction
import LLVM.AST.Operand
import LLVM.Context
import LLVM.Module
import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as BS
import Data.String (fromString)
import Control.Monad.State
import Data.Word

-- Состояние кодогенератора
data CodegenState = CodegenState
    { currentBlock :: L.Name                    -- Текущий блок
    , blocks :: Map.Map L.Name BlockState       -- Все блоки
    , symtab :: Map.Map String Operand          -- Таблица символов
    , blockCount :: Int                         -- Счетчик блоков
    , count :: Word                             -- Счетчик временных переменных
    , names :: Map.Map String Int               -- Счетчик имен
    } deriving Show

data BlockState = BlockState
    { idx :: Int                                -- Индекс блока
    , stack :: [Named Instruction]              -- Стек инструкций
    , term :: Maybe (Named Terminator)          -- Терминатор блока
    } deriving Show

type Codegen = State CodegenState

-- Генерация LLVM модуля из Program
codegenProgram :: Program -> L.Module
codegenProgram (Program name decls) =
    L.defaultModule
        { L.moduleName = fromString name
        , L.moduleDefinitions = map codegenDecl decls
        }

-- Генерация декларации
codegenDecl :: Decl -> L.Definition
codegenDecl (FunDecl name params retTy body) =
    let llvmRetTy = toLLVMType retTy
        llvmParams = map (\(n, ty) -> L.Parameter (toLLVMType ty) (L.Name $ fromString n) []) params
        funBlocks = evalState (codegenFunction params body) emptyCodegen
    in L.GlobalDefinition $ L.functionDefaults
        { LG.name = L.Name (fromString name)
        , LG.parameters = (llvmParams, False)
        , LG.returnType = llvmRetTy
        , LG.basicBlocks = funBlocks
        }

codegenDecl (RecordDecl name fields) =
    -- Для простоты, пока просто игнорируем record декларации
    -- В будущем можно генерировать struct типы
    L.GlobalDefinition $ L.globalVariableDefaults
        { LG.name = L.Name (fromString $ "_record_" ++ name)
        , LG.type' = LT.i32
        , LG.initializer = Just $ LC.Int 32 0
        }

codegenDecl (EnumDecl name variants) =
    -- Для простоты, enum представляем как константы
    L.GlobalDefinition $ L.globalVariableDefaults
        { LG.name = L.Name (fromString $ "_enum_" ++ name)
        , LG.type' = LT.i32
        , LG.initializer = Just $ LC.Int 32 0
        }

-- Генерация функции
codegenFunction :: [(String, Type)] -> Expr -> Codegen [L.BasicBlock]
codegenFunction params body = do
    entry <- addBlock "entry"
    setBlock entry

    -- Добавляем параметры в таблицу символов
    forM_ params $ \(n, ty) -> do
        let op = LocalReference (toLLVMType ty) (L.Name $ fromString n)
        assign n op

    -- Генерируем тело функции
    result <- codegenExpr body

    -- Добавляем return
    term <- ret result

    -- Получаем все блоки
    allBlocks <- gets blocks
    return $ createBlocks allBlocks

-- Генерация выражения
codegenExpr :: Expr -> Codegen Operand
codegenExpr (EVar name) = do
    syms <- gets symtab
    case Map.lookup name syms of
        Just op -> return op
        Nothing -> error $ "Undefined variable: " ++ name

codegenExpr (ENum n) =
    return $ ConstantOperand $ LC.Int 64 (fromIntegral n)

codegenExpr (EStr s) = do
    -- Строки - это указатели на i8
    -- Для простоты возвращаем null pointer
    return $ ConstantOperand $ LC.Null LT.i8

codegenExpr (EBool b) =
    return $ ConstantOperand $ LC.Int 1 (if b then 1 else 0)

codegenExpr (EBinOp op left right) = do
    l <- codegenExpr left
    r <- codegenExpr right
    case op of
        Add -> instr LT.i64 $ Add False False l r []
        Sub -> instr LT.i64 $ Sub False False l r []
        Mul -> instr LT.i64 $ Mul False False l r []
        Div -> instr LT.i64 $ SDiv False l r []
        Mod -> instr LT.i64 $ SRem l r []
        Eq  -> instr LT.i1 $ ICmp LIP.EQ l r []
        Neq -> instr LT.i1 $ ICmp LIP.NE l r []
        Lt  -> instr LT.i1 $ ICmp LIP.SLT l r []
        Gt  -> instr LT.i1 $ ICmp LIP.SGT l r []
        Le  -> instr LT.i1 $ ICmp LIP.SLE l r []
        Ge  -> instr LT.i1 $ ICmp LIP.SGE l r []
        And -> instr LT.i1 $ LLVM.AST.Instruction.And l r []
        Or  -> instr LT.i1 $ LLVM.AST.Instruction.Or l r []

codegenExpr (EUnOp op expr) = do
    e <- codegenExpr expr
    case op of
        Neg -> do
            zero <- return $ ConstantOperand $ LC.Int 64 0
            instr LT.i64 $ Sub False False zero e []
        Not -> do
            true <- return $ ConstantOperand $ LC.Int 1 1
            instr LT.i1 $ Xor e true []

codegenExpr (ECall fname args) = do
    argOps <- mapM codegenExpr args
    instr LT.i64 $ Call Nothing LCC.C []
        (Right $ ConstantOperand $ LC.GlobalReference
            (LT.ptr $ LT.FunctionType LT.i64 (replicate (length args) LT.i64) False)
            (L.Name $ fromString fname))
        [(op, []) | op <- argOps]
        []
        []

codegenExpr (EIf cond thenE elseE) = do
    condOp <- codegenExpr cond

    thenBlock <- addBlock "if.then"
    elseBlock <- addBlock "if.else"
    mergeBlock <- addBlock "if.merge"

    -- Условный переход
    cbr condOp thenBlock elseBlock

    -- Then блок
    setBlock thenBlock
    thenVal <- codegenExpr thenE
    br mergeBlock
    thenBlock' <- gets currentBlock

    -- Else блок
    setBlock elseBlock
    elseVal <- codegenExpr elseE
    br mergeBlock
    elseBlock' <- gets currentBlock

    -- Merge блок
    setBlock mergeBlock
    phi LT.i64 [(thenVal, thenBlock'), (elseVal, elseBlock')]

codegenExpr (ELet name val body) = do
    valOp <- codegenExpr val
    assign name valOp
    codegenExpr body

codegenExpr (ELambda params body) = do
    -- Lambda требует более сложной реализации
    -- Для простоты возвращаем null
    return $ ConstantOperand $ LC.Null LT.i64

-- Конвертация типов AST в LLVM типы
toLLVMType :: Type -> LT.Type
toLLVMType TyNum = LT.i64
toLLVMType TyLogic = LT.i1
toLLVMType TyText = LT.ptr LT.i8
toLLVMType (TyAction _) = LT.void
toLLVMType (TyCustom _) = LT.i64  -- Упрощение
toLLVMType (TyFun params ret) =
    LT.FunctionType (toLLVMType ret) (map toLLVMType params) False

-- Вспомогательные функции для генерации кода
emptyCodegen :: CodegenState
emptyCodegen = CodegenState
    { currentBlock = L.Name "entry"
    , blocks = Map.empty
    , symtab = Map.empty
    , blockCount = 1
    , count = 0
    , names = Map.empty
    }

fresh :: Codegen Word
fresh = do
    i <- gets count
    modify $ \s -> s { count = i + 1 }
    return $ i + 1

addBlock :: String -> Codegen L.Name
addBlock bname = do
    bls <- gets blocks
    ix <- gets blockCount
    let new = emptyBlock ix
        name = L.Name (fromString bname)
    modify $ \s -> s
        { blocks = Map.insert name new bls
        , blockCount = ix + 1
        }
    return name

emptyBlock :: Int -> BlockState
emptyBlock i = BlockState i [] Nothing

setBlock :: L.Name -> Codegen ()
setBlock bname = modify $ \s -> s { currentBlock = bname }

getBlock :: Codegen L.Name
getBlock = gets currentBlock

modifyBlock :: BlockState -> Codegen ()
modifyBlock new = do
    active <- gets currentBlock
    modify $ \s -> s { blocks = Map.insert active new (blocks s) }

current :: Codegen BlockState
current = do
    c <- gets currentBlock
    blks <- gets blocks
    case Map.lookup c blks of
        Just x -> return x
        Nothing -> error $ "No such block: " ++ show c

instr :: LT.Type -> Instruction -> Codegen Operand
instr ty ins = do
    n <- fresh
    let ref = LocalReference ty (L.UnName n)
    blk <- current
    modifyBlock $ blk { stack = stack blk ++ [L.UnName n := ins] }
    return ref

terminator :: Named Terminator -> Codegen ()
terminator trm = do
    blk <- current
    modifyBlock $ blk { term = Just trm }

ret :: Operand -> Codegen (Named Terminator)
ret val = do
    terminator $ Do $ Ret (Just val) []
    return $ Do $ Ret (Just val) []

br :: L.Name -> Codegen (Named Terminator)
br target = do
    terminator $ Do $ Br target []
    return $ Do $ Br target []

cbr :: Operand -> L.Name -> L.Name -> Codegen (Named Terminator)
cbr cond tr fl = do
    terminator $ Do $ CondBr cond tr fl []
    return $ Do $ CondBr cond tr fl []

phi :: LT.Type -> [(Operand, L.Name)] -> Codegen Operand
phi ty incoming = instr ty $ Phi ty incoming []

assign :: String -> Operand -> Codegen ()
assign var x = modify $ \s -> s { symtab = Map.insert var x (symtab s) }

createBlocks :: Map.Map L.Name BlockState -> [L.BasicBlock]
createBlocks blks = map makeBlock $ Map.toList blks
  where
    makeBlock (l, BlockState _ s t) = L.BasicBlock l s (maketerm t)
    maketerm (Just x) = x
    maketerm Nothing = error "Block has no terminator"

-- Экспорт LLVM IR в файл
generateLLVM :: Program -> IO String
generateLLVM prog = do
    let llvmModule = codegenProgram prog
    withContext $ \ctx ->
        withModuleFromAST ctx llvmModule $ \m ->
            moduleLLVMAssembly m

-- Сохранение LLVM IR в файл
saveLLVMToFile :: Program -> FilePath -> IO ()
saveLLVMToFile prog filepath = do
    llvmIR <- generateLLVM prog
    writeFile filepath llvmIR
    putStrLn $ "LLVM IR saved to: " ++ filepath

