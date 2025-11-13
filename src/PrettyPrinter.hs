module PrettyPrinter where

import AST
import Data.List (intercalate)

-- Pretty print всей программы
prettyProgram :: Program -> String
prettyProgram (Program name decls) =
    "Module: " ++ name ++ "\n\n" ++
    intercalate "\n\n" (map prettyDecl decls)

-- Pretty print декларации
prettyDecl :: Decl -> String
prettyDecl (FunDecl name params retTy body) =
    "Function Declaration:\n" ++
    "  Name: " ++ name ++ "\n" ++
    "  Parameters: " ++ prettyParams params ++ "\n" ++
    "  Return Type: " ++ prettyType retTy ++ "\n" ++
    "  Body: " ++ prettyExpr body

prettyDecl (RecordDecl name fields) =
    "Record Declaration:\n" ++
    "  Name: " ++ name ++ "\n" ++
    "  Fields:\n" ++
    indent 4 (intercalate "\n" (map prettyField fields))

prettyDecl (EnumDecl name variants) =
    "Enum Declaration:\n" ++
    "  Name: " ++ name ++ "\n" ++
    "  Variants: " ++ intercalate ", " variants

-- Pretty print типов
prettyType :: Type -> String
prettyType TyNum = "Num"
prettyType TyLogic = "Logic"
prettyType TyText = "Text"
prettyType (TyAction Nothing) = "Action"
prettyType (TyAction (Just t)) = "Action<" ++ prettyType t ++ ">"
prettyType (TyCustom name) = name
prettyType (TyFun params ret) =
    "(" ++ intercalate ", " (map prettyType params) ++ ") -> " ++ prettyType ret

-- Pretty print параметров
prettyParams :: [(String, Type)] -> String
prettyParams [] = "(none)"
prettyParams params = intercalate ", " (map prettyParam params)

prettyParam :: (String, Type) -> String
prettyParam (name, ty) = name ++ ": " ++ prettyType ty

-- Pretty print полей
prettyField :: Field -> String
prettyField (Field name ty) = name ++ ": " ++ prettyType ty

-- Pretty print выражений
prettyExpr :: Expr -> String
prettyExpr (EVar name) = name
prettyExpr (ENum n) = show n
prettyExpr (EStr s) = show s
prettyExpr (EBool b) = if b then "True" else "False"
prettyExpr (EBinOp op left right) =
    "(" ++ prettyExpr left ++ " " ++ prettyBinOp op ++ " " ++ prettyExpr right ++ ")"
prettyExpr (EUnOp op expr) =
    prettyUnOp op ++ prettyExpr expr
prettyExpr (ECall func args) =
    func ++ "(" ++ intercalate ", " (map prettyExpr args) ++ ")"
prettyExpr (EIf cond thenE elseE) =
    "if " ++ prettyExpr cond ++
    " then " ++ prettyExpr thenE ++
    " else " ++ prettyExpr elseE
prettyExpr (ELet name val body) =
    "let " ++ name ++ " = " ++ prettyExpr val ++ " in " ++ prettyExpr body
prettyExpr (ELambda params body) =
    "λ(" ++ intercalate ", " (map prettyParam params) ++ ") -> " ++ prettyExpr body

-- Pretty print операторов
prettyBinOp :: BinOp -> String
prettyBinOp Add = "+"
prettyBinOp Sub = "-"
prettyBinOp Mul = "*"
prettyBinOp Div = "/"
prettyBinOp Mod = "%"
prettyBinOp Eq = "=="
prettyBinOp Neq = "!="
prettyBinOp Lt = "<"
prettyBinOp Gt = ">"
prettyBinOp Le = "<="
prettyBinOp Ge = ">="
prettyBinOp And = "&&"
prettyBinOp Or = "||"

prettyUnOp :: UnOp -> String
prettyUnOp Neg = "-"
prettyUnOp Not = "!"

-- Вспомогательная функция для отступов
indent :: Int -> String -> String
indent n str = unlines $ map (replicate n ' ' ++) (lines str)
