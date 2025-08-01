module AST (Expr) where

data Expr 
  = ENumber Integer
  | EString String
  | EBool Bool 
  | EIdentifier String
  | EBinaryOp String Expr Expr  
  | EIf Expr Expr Expr         
  | ELet String Expr Expr     
  deriving (Show, Eq)
