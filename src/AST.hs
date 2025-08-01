module AST (Expr(..), Type, Definition(..), Program(..)) where

type Type = String

data Expr
  = ENumber Integer         
  | EString String           
  | EBool Bool                
  | EIdentifier String         
  | EBinaryOp String Expr Expr  
  | EIf Expr Expr Expr           
  | ELet String Expr Expr       
  | EFunctionCall String [Expr]  
  | EBlock [Expr]                 
  deriving (Show, Eq)

data Definition
  = DFunctionDef String [(String, Type)] Type Expr 
  | DMainBlock Expr                                 
  deriving (Show, Eq)

data Program
  = PProgram String [Definition]  
  deriving (Show, Eq)
