module SemanticAnalyzer where

import AST
import qualified Data.Map as Map
import Control.Monad (when, unless)

type SymbolTable = Map.Map String Type
type ErrorList = [String]

data AnalysisResult = AnalysisResult
    { errors :: ErrorList
    , warnings :: ErrorList
    } deriving (Show)

analyzeProgram :: Program -> AnalysisResult
analyzeProgram (Program _ stmts) =
    let (errs, warns) = analyzeStmts Map.empty stmts
    in AnalysisResult errs warns

analyzeStmts :: SymbolTable -> [Stmt] -> (ErrorList, ErrorList)
analyzeStmts symTable stmts =
    foldl processStmt ([], []) stmts
  where
    processStmt (errs, warns) stmt =
        let (newErrs, newWarns, _) = analyzeStmt symTable stmt
        in (errs ++ newErrs, warns ++ newWarns)

analyzeStmt :: SymbolTable -> Stmt -> (ErrorList, ErrorList, SymbolTable)
analyzeStmt symTable (VarDecl name ty expr) =
    let (errs, warns) = checkExpr symTable expr
        newSymTable = Map.insert name ty symTable
        declErrs = if Map.member name symTable
                   then ["Variable '" ++ name ++ "' is already declared"]
                   else []
    in (errs ++ declErrs, warns, newSymTable)

analyzeStmt symTable (FunDecl name params retTy body) =
    let paramSymTable = foldl (\st (pName, pTy) -> Map.insert pName pTy st) symTable params
        (errs, warns) = checkExpr paramSymTable body
        newSymTable = Map.insert name (TyCustom "Function") symTable
        declErrs = if Map.member name symTable
                   then ["Function '" ++ name ++ "' is already declared"]
                   else []
    in (errs ++ declErrs, warns, newSymTable)

analyzeStmt symTable (ActionDecl name body) =
    let (errs, warns) = analyzeStmts symTable body
        newSymTable = Map.insert name (TyCustom "Action") symTable
    in (errs, warns, newSymTable)

analyzeStmt symTable (RecordDecl name fields) =
    let fieldNames = map (\(Field n _) -> n) fields
        duplicates = findDuplicates fieldNames
        errs = if null duplicates
               then []
               else ["Duplicate fields in record '" ++ name ++ "': " ++ show duplicates]
        newSymTable = Map.insert name (TyCustom "Record") symTable
    in (errs, [], newSymTable)

analyzeStmt symTable (EnumDecl name variants) =
    let duplicates = findDuplicates variants
        errs = if null duplicates
               then []
               else ["Duplicate variants in enum '" ++ name ++ "': " ++ show duplicates]
        newSymTable = Map.insert name (TyCustom "Enum") symTable
    in (errs, [], newSymTable)

analyzeStmt symTable (ExprStmt expr) =
    let (errs, warns) = checkExpr symTable expr
    in (errs, warns, symTable)

checkExpr :: SymbolTable -> Expr -> (ErrorList, ErrorList)
checkExpr symTable (EVar name) =
    if Map.member name symTable
    then ([], [])
    else (["Undefined variable: " ++ name], [])

checkExpr _ (ENum _) = ([], [])
checkExpr _ (EStr _) = ([], [])

checkExpr symTable (EBinOp op left right) =
    let (leftErrs, leftWarns) = checkExpr symTable left
        (rightErrs, rightWarns) = checkExpr symTable right
    in (leftErrs ++ rightErrs, leftWarns ++ rightWarns)

checkExpr symTable (ECall func args) =
    let funcCheck = if Map.member func symTable
                    then ([], [])
                    else (["Undefined function: " ++ func], [])
        (argErrs, argWarns) = foldl (\(e, w) arg ->
            let (e2, w2) = checkExpr symTable arg
            in (e ++ e2, w ++ w2)) ([], []) args
    in (fst funcCheck ++ argErrs, snd funcCheck ++ argWarns)

findDuplicates :: Eq a => [a] -> [a]
findDuplicates [] = []
findDuplicates (x:xs)
    | x `elem` xs = x : findDuplicates xs
    | otherwise = findDuplicates xs

hasErrors :: AnalysisResult -> Bool
hasErrors = not . null . errors

printAnalysis :: AnalysisResult -> IO ()
printAnalysis result = do
    unless (null $ errors result) $ do
        putStrLn "=== ERRORS ==="
        mapM_ (\e -> putStrLn $ "  ❌ " ++ e) (errors result)
        putStrLn ""

    unless (null $ warnings result) $ do
        putStrLn "=== WARNINGS ==="
        mapM_ (\w -> putStrLn $ "  ⚠️  " ++ w) (warnings result)
        putStrLn ""

    when (null (errors result) && null (warnings result)) $
        putStrLn "✅ No errors or warnings found!"

