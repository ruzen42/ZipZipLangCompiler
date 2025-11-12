module AST where

data Program = Program String [Stmt]
    deriving (Show, Eq)

data Stmt
    = VarDecl String Type Expr
    | FunDecl String [(String, Type)] Type Expr
    | ActionDecl String [Stmt]
    | ExprStmt Expr
    | RecordDecl String [Field]
    | EnumDecl String [String]
    deriving (Show, Eq)

data Field = Field String Type
    deriving (Show, Eq)

data Type = TyNum | TyText | TyCustom String
    deriving (Show, Eq)

data Expr
    = EVar String
    | ENum Int
    | EStr String
    | EBinOp String Expr Expr
    | ECall String [Expr]
    deriving (Show, Eq)
