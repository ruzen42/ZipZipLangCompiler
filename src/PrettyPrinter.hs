module PrettyPrinter where

import AST
import Data.List (intercalate)

prettyProgram :: Program -> String
prettyProgram (Program name stmts) =
    "Module: " ++ name ++ "\n\n" ++
    intercalate "\n\n" (map prettyStmt stmts)

prettyStmt :: Stmt -> String
prettyStmt (VarDecl name ty expr) =
    "Variable Declaration:\n" ++
    "  Name: " ++ name ++ "\n" ++
    "  Type: " ++ prettyType ty ++ "\n" ++
    "  Value: " ++ prettyExpr expr

prettyStmt (FunDecl name params retTy body) =
    "Function Declaration:\n" ++
    "  Name: " ++ name ++ "\n" ++
    "  Parameters: " ++ intercalate ", " (map prettyParam params) ++ "\n" ++
    "  Return Type: " ++ prettyType retTy ++ "\n" ++
    "  Body: " ++ prettyExpr body

prettyStmt (ActionDecl name body) =
    "Action Declaration:\n" ++
    "  Name: " ++ name ++ "\n" ++
    "  Statements:\n" ++
    indent 4 (intercalate "\n" (map prettyStmt body))

prettyStmt (RecordDecl name fields) =
    "Record Declaration:\n" ++
    "  Name: " ++ name ++ "\n" ++
    "  Fields:\n" ++
    indent 4 (intercalate "\n" (map prettyField fields))

prettyStmt (EnumDecl name variants) =
    "Enum Declaration:\n" ++
    "  Name: " ++ name ++ "\n" ++
    "  Variants: " ++ intercalate ", " variants

prettyStmt (ExprStmt expr) =
    "Expression Statement:\n" ++
    "  " ++ prettyExpr expr

prettyType :: Type -> String
prettyType TyNum = "Num"
prettyType TyText = "Text"
prettyType (TyCustom name) = name

prettyParam :: (String, Type) -> String
prettyParam (name, ty) = name ++ ": " ++ prettyType ty

prettyField :: Field -> String
prettyField (Field name ty) = name ++ ": " ++ prettyType ty

prettyExpr :: Expr -> String
prettyExpr (EVar name) = name
prettyExpr (ENum n) = show n
prettyExpr (EStr s) = show s
prettyExpr (EBinOp op left right) =
    "(" ++ prettyExpr left ++ " " ++ op ++ " " ++ prettyExpr right ++ ")"
prettyExpr (ECall func args) =
    func ++ "(" ++ intercalate ", " (map prettyExpr args) ++ ")"

indent :: Int -> String -> String
indent n str = unlines $ map (replicate n ' ' ++) (lines str)
