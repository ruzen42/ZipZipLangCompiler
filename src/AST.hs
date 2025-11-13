module AST where

-- Программа состоит из имени модуля и списка деклараций
data Program = Program String [Decl]
    deriving (Show, Eq)

-- Декларации верхнего уровня
data Decl
    = FunDecl String [(String, Type)] Type Expr  -- функция (может быть без параметров - константа)
    | RecordDecl String [Field]                  -- record тип
    | EnumDecl String [String]                   -- enum тип
    deriving (Show, Eq)

-- Поле записи
data Field = Field String Type
    deriving (Show, Eq)

-- Типы
data Type
    = TyNum                    -- числовой тип
    | TyLogic                  -- булевый тип (Logic)
    | TyText                   -- текстовый тип
    | TyAction (Maybe Type)    -- Action или Action<T>
    | TyCustom String          -- пользовательские типы (record/enum)
    | TyFun [Type] Type        -- функциональный тип (для будущего)
    deriving (Show, Eq)

-- Выражения
data Expr
    = EVar String              -- переменная/функция
    | ENum Int                 -- число
    | EStr String              -- строка
    | EBool Bool               -- булево значение
    | EBinOp BinOp Expr Expr   -- бинарная операция
    | EUnOp UnOp Expr          -- унарная операция
    | ECall String [Expr]      -- вызов функции
    | EIf Expr Expr Expr       -- if-then-else
    | ELet String Expr Expr    -- let binding (для будущего)
    | ELambda [(String, Type)] Expr  -- lambda (для будущего)
    deriving (Show, Eq)

-- Бинарные операторы
data BinOp
    = Add | Sub | Mul | Div | Mod     -- арифметические
    | Eq | Neq | Lt | Gt | Le | Ge    -- сравнения
    | And | Or                         -- логические
    deriving (Show, Eq)

-- Унарные операторы
data UnOp
    = Neg    -- арифметическое отрицание
    | Not    -- логическое отрицание
    deriving (Show, Eq)
