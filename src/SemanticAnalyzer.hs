module SemanticAnalyzer where

import AST
import qualified Data.Map as Map
import Control.Monad (when, unless)
import Data.Maybe (isJust, fromMaybe)

type SymbolTable = Map.Map String Type
type ErrorList = [String]

data AnalysisResult = AnalysisResult
    { errors :: ErrorList
    , warnings :: ErrorList
    } deriving (Show)

-- Анализ всей программы
analyzeProgram :: Program -> AnalysisResult
analyzeProgram (Program moduleName decls) =
    let symTable = buildSymbolTable decls
        (errs, warns) = analyzeDecls symTable decls
    in AnalysisResult errs warns

-- Построение таблицы символов (первый проход)
buildSymbolTable :: [Decl] -> SymbolTable
buildSymbolTable = foldl addDecl Map.empty
  where
    addDecl table (FunDecl name _ retTy _) = Map.insert name retTy table
    addDecl table (RecordDecl name _) = Map.insert name (TyCustom name) table
    addDecl table (EnumDecl name _) = Map.insert name (TyCustom name) table

-- Анализ списка деклараций (второй проход)
analyzeDecls :: SymbolTable -> [Decl] -> (ErrorList, ErrorList)
analyzeDecls symTable decls =
    foldl processDecl ([], []) decls
  where
    processDecl (errs, warns) decl =
        let (newErrs, newWarns) = analyzeDecl symTable decl
        in (errs ++ newErrs, warns ++ newWarns)

-- Анализ одной декларации
analyzeDecl :: SymbolTable -> Decl -> (ErrorList, ErrorList)
analyzeDecl symTable (FunDecl name params retTy body) =
    let paramSymTable = foldl (\st (pName, pTy) -> Map.insert pName pTy st) symTable params
        (bodyTy, errs, warns) = inferType paramSymTable body
        typeErrs = case (bodyTy, retTy) of
            (Just bt, rt) | not (typesCompatible bt rt) ->
                ["Function '" ++ name ++ "' return type mismatch: expected " ++
                 show rt ++ ", got " ++ show bt]
            (Nothing, _) -> []  -- Ошибки типов уже в errs
            _ -> []
    in (errs ++ typeErrs, warns)

analyzeDecl symTable (RecordDecl name fields) =
    let fieldNames = map (\(Field n _) -> n) fields
        duplicates = findDuplicates fieldNames
        errs = if null duplicates
               then []
               else ["Duplicate fields in record '" ++ name ++ "': " ++ show duplicates]
    in (errs, [])

analyzeDecl symTable (EnumDecl name variants) =
    let duplicates = findDuplicates variants
        errs = if null duplicates
               then []
               else ["Duplicate variants in enum '" ++ name ++ "': " ++ show duplicates]
    in (errs, [])

-- Вывод типов с проверкой
inferType :: SymbolTable -> Expr -> (Maybe Type, ErrorList, ErrorList)
inferType symTable (EVar name) =
    case Map.lookup name symTable of
        Just ty -> (Just ty, [], [])
        Nothing -> (Nothing, ["Undefined variable/function: " ++ name], [])

inferType _ (ENum _) = (Just TyNum, [], [])
inferType _ (EStr _) = (Just TyText, [], [])
inferType _ (EBool _) = (Just TyLogic, [], [])

inferType symTable (EBinOp op left right) =
    let (leftTy, leftErrs, leftWarns) = inferType symTable left
        (rightTy, rightErrs, rightWarns) = inferType symTable right
        (expectedTy, resultTy) = opTypes op
        errs = leftErrs ++ rightErrs
        warns = leftWarns ++ rightWarns
        typeErrs = case (leftTy, rightTy) of
            (Just lt, Just rt) ->
                let ltErr = if typesCompatible lt expectedTy
                           then []
                           else ["Left operand type mismatch: expected " ++ show expectedTy ++ ", got " ++ show lt]
                    rtErr = if typesCompatible rt expectedTy
                           then []
                           else ["Right operand type mismatch: expected " ++ show expectedTy ++ ", got " ++ show rt]
                in ltErr ++ rtErr
            _ -> []
    in (Just resultTy, errs ++ typeErrs, warns)

inferType symTable (EUnOp op expr) =
    let (exprTy, errs, warns) = inferType symTable expr
        (expectedTy, resultTy) = unOpTypes op
        typeErrs = case exprTy of
            Just et | not (typesCompatible et expectedTy) ->
                ["Unary operator type mismatch: expected " ++ show expectedTy ++ ", got " ++ show et]
            _ -> []
    in (Just resultTy, errs ++ typeErrs, warns)

inferType symTable (ECall func args) =
    let funcTy = Map.lookup func symTable
        (argTys, argErrs, argWarns) = foldl checkArg ([], [], []) args
        checkArg (tys, errs, warns) arg =
            let (ty, e, w) = inferType symTable arg
            in (tys ++ [ty], errs ++ e, warns ++ w)
        funcErrs = case funcTy of
            Nothing -> ["Undefined function: " ++ func]
            Just _ -> []  -- TODO: проверка типов параметров
    in (funcTy, argErrs ++ funcErrs, argWarns)

inferType symTable (EIf cond thenE elseE) =
    let (condTy, condErrs, condWarns) = inferType symTable cond
        (thenTy, thenErrs, thenWarns) = inferType symTable thenE
        (elseTy, elseErrs, elseWarns) = inferType symTable elseE
        condTypeErrs = case condTy of
            Just TyLogic -> []
            Just ty -> ["If condition must be Logic type, got " ++ show ty]
            Nothing -> []
        branchTypeErrs = case (thenTy, elseTy) of
            (Just tt, Just et) | not (typesCompatible tt et) ->
                ["If branches type mismatch: then is " ++ show tt ++ ", else is " ++ show et]
            _ -> []
        resultTy = thenTy  -- Берем тип then ветки
    in (resultTy, condErrs ++ thenErrs ++ elseErrs ++ condTypeErrs ++ branchTypeErrs,
        condWarns ++ thenWarns ++ elseWarns)

inferType symTable (ELet name val body) =
    let (valTy, valErrs, valWarns) = inferType symTable val
        newSymTable = case valTy of
            Just ty -> Map.insert name ty symTable
            Nothing -> Map.insert name (TyCustom "Unknown") symTable
        (bodyTy, bodyErrs, bodyWarns) = inferType newSymTable body
    in (bodyTy, valErrs ++ bodyErrs, valWarns ++ bodyWarns)

inferType symTable (ELambda params body) =
    let paramSymTable = foldl (\st (pName, pTy) -> Map.insert pName pTy st) symTable params
        (bodyTy, errs, warns) = inferType paramSymTable body
        lambdaTy = case bodyTy of
            Just bt -> Just $ TyFun (map snd params) bt
            Nothing -> Nothing
    in (lambdaTy, errs, warns)

-- Типы операторов
opTypes :: BinOp -> (Type, Type)
opTypes Add = (TyNum, TyNum)
opTypes Sub = (TyNum, TyNum)
opTypes Mul = (TyNum, TyNum)
opTypes Div = (TyNum, TyNum)
opTypes Mod = (TyNum, TyNum)
opTypes Eq = (TyNum, TyLogic)  -- Упрощение: только для чисел
opTypes Neq = (TyNum, TyLogic)
opTypes Lt = (TyNum, TyLogic)
opTypes Gt = (TyNum, TyLogic)
opTypes Le = (TyNum, TyLogic)
opTypes Ge = (TyNum, TyLogic)
opTypes And = (TyLogic, TyLogic)
opTypes Or = (TyLogic, TyLogic)

unOpTypes :: UnOp -> (Type, Type)
unOpTypes Neg = (TyNum, TyNum)
unOpTypes Not = (TyLogic, TyLogic)

-- Проверка совместимости типов
typesCompatible :: Type -> Type -> Bool
typesCompatible TyNum TyNum = True
typesCompatible TyText TyText = True
typesCompatible TyLogic TyLogic = True
typesCompatible (TyCustom a) (TyCustom b) = a == b
typesCompatible (TyAction a) (TyAction b) = a == b
typesCompatible _ _ = False

-- Вспомогательные функции
findDuplicates :: Eq a => [a] -> [a]
findDuplicates [] = []
findDuplicates (x:xs)
    | x `elem` xs = x : findDuplicates (filter (/= x) xs) ++ [x]
    | otherwise = findDuplicates xs

-- Проверка результата анализа
hasErrors :: AnalysisResult -> Bool
hasErrors = not . null . errors

printAnalysis :: AnalysisResult -> IO ()
printAnalysis result = do
    unless (null $ errors result) $ do
        putStrLn "ERRORS: "
        mapM_ (\e -> putStrLn $ e) (errors result)
        putStrLn ""

    unless (null $ warnings result) $ do
        putStrLn "WARNINGS: "
        mapM_ (\w -> putStrLn $ w) (warnings result)
        putStrLn ""

    when (null (errors result) && null (warnings result)) $
        putStrLn "No errors or warnings found!"

