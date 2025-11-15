module AST where

data Program = Program String [Decl]
    deriving (Show, Eq)

data Decl
    = FunDecl String [(String, Type)] Type Expr
    | RecordDecl String [Field]
    | EnumDecl String [String]
    deriving (Show, Eq)

data Field = Field String Type
    deriving (Show, Eq)

data Type
    = TyNum
    | TyLogic
    | TyText
    | TyAction (Maybe Type)
    | TyCustom String
    | TyFun [Type] Type
    deriving (Show, Eq)

-- Выражения
data Expr
    = EVar String
    | ENum Int
    | EStr String
    | EBool Bool
    | EBinOp BinOp Expr Expr
    | EUnOp UnOp Expr
    | ECall String [Expr]
    | EIf Expr Expr Expr
    | ELet String Expr Expr
    | ELambda [(String, Type)] Expr
    deriving (Show, Eq)

data BinOp
    = Add | Sub | Mul | Div | Mod
    | Eq | Neq | Lt | Gt | Le | Ge
    | And | Or
    deriving (Show, Eq)

data UnOp
    = Neg
    | Not
    deriving (Show, Eq)
