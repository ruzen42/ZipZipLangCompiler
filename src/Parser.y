{
module Parser where

import AST
import Lexer
}

%name parse
%tokentype { Token }
%error { parseError }

%token
  in        { TokIn }
  record    { TokRecord }
  enum      { TokEnum }
  Action    { TokAction }
  Num       { TokNum }
  Text      { TokText }
  ':'       { TokColon }
  ','       { TokComma }
  ';'       { TokSemicolon }
  '='       { TokEq }
  '->'      { TokArrow }
  '+'       { TokPlus }
  '-'       { TokMinus }
  '*'       { TokStar }
  '|'       { TokPipe }
  '('       { TokLParen }
  ')'       { TokRParen }
  ident     { TokIdent $$ }
  number    { TokNumber $$ }
  string    { TokString $$ }

%left '+' '-'
%left '*'

%%

Program : in ident Stmts                { Program $2 $3 }

Stmts : {- empty -}                     { [] }
      | Stmt Stmts                      { $1 : $2 }

Stmt : ident Type ':' Expr              { VarDecl $1 $2 $4 }
     | Type ident Params ':' Expr       { FunDecl $2 $3 $1 $5 }
     | Action ident ':' ActionBody      { ActionDecl $2 $4 }
     | record ident ':' Fields          { RecordDecl $2 $4 }
     | enum ident ':' EnumVariants      { EnumDecl $2 $4 }

Params : {- empty -}                    { [] }
       | ParamList                      { $1 }

ParamList : Type ident                  { [($2, $1)] }
          | Type ident ',' ParamList    { ($2, $1) : $4 }

ActionBody : {- empty -}                { [] }
           | ActionStmt ActionBody      { $1 : $2 }

ActionStmt : Type ident '=' Expr ';'    { VarDecl $2 $1 $4 }
           | Expr ';'                   { ExprStmt $1 }

Fields : Field                          { [$1] }
       | Field ',' Fields               { $1 : $3 }

Field : ident Type                      { Field $1 $2 }

EnumVariants : ident                    { [$1] }
             | ident '|' EnumVariants   { $1 : $3 }

Type : Num                              { TyNum }
     | Text                             { TyText }
     | ident                            { TyCustom $1 }

Expr : Term                             { $1 }
     | Expr '+' Expr                    { EBinOp "+" $1 $3 }
     | Expr '-' Expr                    { EBinOp "-" $1 $3 }
     | Expr '*' Expr                    { EBinOp "*" $1 $3 }

Term : ident                            { EVar $1 }
     | number                           { ENum $1 }
     | string                           { EStr $1 }
     | ident '(' Args ')'               { ECall $1 $3 }
     | '(' Expr ')'                     { $2 }

Args : {- empty -}                      { [] }
     | ArgList                          { $1 }

ArgList : Expr                          { [$1] }
        | Expr ',' ArgList              { $1 : $3 }

{

parseError :: [Token] -> a
parseError [] = error "Parse error: unexpected end of input"
parseError tokens = error $ unlines
  [ "Parse error!"
  , "Expected valid statement or expression"
  , "Got: " ++ show (head tokens)
  , "Context: " ++ show (take 5 tokens)
  , ""
  , "Hint: Check for:"
  , "  - Missing semicolons"
  , "  - Incorrect type declarations"
  , "  - Malformed expressions"
  ]

}
