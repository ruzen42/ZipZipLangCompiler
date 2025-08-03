{-# LANGUAGE FlexibleContexts #-}

module SemanticAnalyzer (SemanticError(..), analyzeProgram, analyzeExpr, eAnalyzeExpr) where

import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Except
import Control.Monad

import AST

data SemanticError
  = UndefinedVariable String
  | TypeMismatch String String String  
  | UndefinedFunction String
  | ArityMismatch String Int Int      
  | DuplicateDefinition String
  | InvalidMainBlock String
  | InvalidBinaryOperation String String String  
  deriving (Eq)

instance Show SemanticError where
  show (UndefinedVariable var) = "Undefined variable: " ++ var
  show (TypeMismatch expected actual context) = 
    "Type mismatch in " ++ context ++ ": expected " ++ expected ++ ", got " ++ actual
  show (UndefinedFunction func) = "Undefined function: " ++ func
  show (ArityMismatch func expected actual) = 
    "Arity mismatch for function " ++ func ++ ": expected " ++ show expected ++ " arguments, got " ++ show actual
  show (DuplicateDefinition name) = "Duplicate definition: " ++ name
  show (InvalidMainBlock msg) = "Invalid main block: " ++ msg
  show (InvalidBinaryOperation op leftType rightType) = 
    "Invalid binary operation " ++ op ++ " between " ++ leftType ++ " and " ++ rightType

data SymbolInfo = SymbolInfo
  { symbolType :: Type
  , symbolArity :: Maybe Int  
  } deriving (Show, Eq)

type SymbolTable = Map.Map String SymbolInfo
type SemanticAnalyzer = StateT SymbolTable (Either SemanticError)

builtInFunctions :: SymbolTable
builtInFunctions = Map.fromList
  [ ("print", SymbolInfo "Text -> ()" (Just 1))
  , ("toText", SymbolInfo "Num -> Text" (Just 1))
  , ("length", SymbolInfo "Text -> Num" (Just 1))
  , ("zip", SymbolInfo "Text -> Text -> Text" (Just 2))
  ]

analyzeProgram :: Program -> Either SemanticError SymbolTable
analyzeProgram (PProgram _ definitions) = do
  (_, finalTable) <- runStateT (analyzeDefinitions definitions) builtInFunctions
  return finalTable

analyzeDefinitions :: [Definition] -> SemanticAnalyzer ()
analyzeDefinitions definitions = do
  mapM_ collectSignature definitions
  mapM_ analyzeDefinition definitions
  where
    collectSignature (DFunctionDef name params returnType _) = do
      table <- get
      case Map.lookup name table of
        Just _ -> throwError (DuplicateDefinition name)
        Nothing -> do
          let arity = length params
          let funcType = foldr (\(_, paramType) acc -> paramType ++ " -> " ++ acc) returnType params
          put $ Map.insert name (SymbolInfo funcType (Just arity)) table
    collectSignature (DMainBlock _) = return ()

analyzeDefinition :: Definition -> SemanticAnalyzer ()
analyzeDefinition (DFunctionDef name params returnType body) = do
  oldTable <- get
  let paramTable = Map.fromList [(pName, SymbolInfo pType Nothing) | (pName, pType) <- params]
  put $ Map.union paramTable oldTable
  
  bodyType <- analyzeExpr body
  when (bodyType /= returnType) $
    throwError (TypeMismatch returnType bodyType ("function " ++ name))
  
  put oldTable

analyzeDefinition (DMainBlock body) = do
  _ <- analyzeExpr body
  return ()

analyzeExpr :: Expr -> SemanticAnalyzer Type
analyzeExpr (ENumber _) = return "Num"
analyzeExpr (EString _) = return "Text"
analyzeExpr (EBool _) = return "Logic"

analyzeExpr (EIdentifier var) = do
  table <- get
  case Map.lookup var table of
    Just (SymbolInfo varType Nothing) -> return varType
    Just (SymbolInfo _ (Just _)) -> throwError (TypeMismatch "variable" "function" var)
    Nothing -> throwError (UndefinedVariable var)

analyzeExpr (EBinaryOp op left right) = do
  leftType <- analyzeExpr left
  rightType <- analyzeExpr right
  checkBinaryOperation op leftType rightType

analyzeExpr (EIf cond thenExpr elseExpr) = do
  condType <- analyzeExpr cond
  when (condType /= "Logic") $
    throwError (TypeMismatch "Logic" condType "if condition")
  
  thenType <- analyzeExpr thenExpr
  elseType <- analyzeExpr elseExpr
  
  when (thenType /= elseType) $
    throwError (TypeMismatch thenType elseType "if-else branches")
  
  return thenType

analyzeExpr (ELet var bindExpr body) = do
  bindType <- analyzeExpr bindExpr
  oldTable <- get
  put $ Map.insert var (SymbolInfo bindType Nothing) oldTable
  bodyType <- analyzeExpr body
  put oldTable
  return bodyType

analyzeExpr (EFunctionCall funcName args) = do
  table <- get
  case Map.lookup funcName table of
    Nothing -> throwError (UndefinedFunction funcName)
    Just (SymbolInfo funcType (Just expectedArity)) -> do
      let actualArity = length args
      when (expectedArity /= actualArity) $
        throwError (ArityMismatch funcName expectedArity actualArity)
      
      argTypes <- mapM analyzeExpr args
      
      let (expectedParamTypes, returnType) = parseFunctionType funcType
      
      zipWithM_ (\expected actual -> 
        when (expected /= actual) $
          throwError (TypeMismatch expected actual ("argument to " ++ funcName))
        ) expectedParamTypes argTypes
      
      return returnType
    
    Just (SymbolInfo _ Nothing) -> 
      throwError (TypeMismatch "function" "variable" funcName)

analyzeExpr (EBlock exprs) = do
  case exprs of
    [] -> return "()"  
    _ -> do
      types <- mapM analyzeExpr exprs
      return (last types)

checkBinaryOperation :: String -> Type -> Type -> SemanticAnalyzer Type
checkBinaryOperation "+" "Num" "Num" = return "Num"
checkBinaryOperation "++" "Text" "Text" = return "Text"
checkBinaryOperation "*" "Num" "Num" = return "Num"
checkBinaryOperation "/" "Num" "Num" = return "Num"
checkBinaryOperation "==" leftType rightType 
  | leftType == rightType = return "Logic"
checkBinaryOperation "&&" "Logic" "Logic" = return "Logic"
checkBinaryOperation "||" "Logic" "Logic" = return "Logic"
checkBinaryOperation op leftType rightType = 
  throwError (InvalidBinaryOperation op leftType rightType)

parseFunctionType :: String -> ([Type], Type)
parseFunctionType funcType = 
  let parts = words $ filter (/= '-') $ filter (/= '>') funcType
  in case reverse parts of
    (returnType:paramTypes) -> (reverse paramTypes, returnType)
    [] -> ([], "()")

eAnalyzeExpr :: Expr -> Either SemanticError Type
eAnalyzeExpr expr = evalStateT (SemanticAnalyzer.analyzeExpr expr) builtInFunctions
