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
  if        { TokIf }
  then      { TokThen }
  else      { TokElse }
  let       { TokLet }
  true      { TokTrue }
  false     { TokFalse }
  Action    { TokAction }
  Num       { TokNum }
  Text      { TokText }
  Logic     { TokLogic }
  ':'       { TokColon }
  ','       { TokComma }
  ';'       { TokSemicolon }
  '='       { TokEq }
  '=='      { TokEqEq }
  '!='      { TokNeq }
  '<'       { TokLt }
  '>'       { TokGt }
  '<='      { TokLe }
  '>='      { TokGe }
  '->'      { TokArrow }
  '+'       { TokPlus }
  '-'       { TokMinus }
  '*'       { TokStar }
  '/'       { TokSlash }
  '%'       { TokPercent }
  '&&'      { TokAnd }
  '||'      { TokOr }
  '!'       { TokNot }
  '|'       { TokPipe }
  '('       { TokLParen }
  ')'       { TokRParen }
  '{'       { TokLBrace }
  '}'       { TokRBrace }
  ident     { TokIdent $$ }
  number    { TokNumber $$ }
  string    { TokString $$ }

%right '->'
%left '||'
%left '&&'
%nonassoc '==' '!=' '<' '>' '<=' '>='
%left '+' '-'
%left '*' '/' '%'
%right '!'
%right NEG

%%

Program : in ident Decls                { Program $2 $3 }

Decls : {- empty -}                     { [] }
      | Decl Decls                      { $1 : $2 }

Decl : Type ident Params ':' Expr       { FunDecl $2 $3 $1 $5 }
     | record ident ':' Fields          { RecordDecl $2 $4 }
     | enum ident ':' EnumVariants      { EnumDecl $2 $4 }

Params : {- empty -}                    { [] }
       | ParamList                      { $1 }

ParamList : Type ident                  { [($2, $1)] }
          | Type ident ',' ParamList    { ($2, $1) : $4 }

Fields : Field                          { [$1] }
       | Field ',' Fields               { $1 : $3 }

Field : ident Type                      { Field $1 $2 }

EnumVariants : ident                    { [$1] }
             | ident '|' EnumVariants   { $1 : $3 }

Type : Num                              { TyNum }
     | Text                             { TyText }
     | Logic                            { TyLogic }
     | Action                           { TyAction Nothing }
     | ident                            { TyCustom $1 }

Expr : Term                             { $1 }
     | Expr '+' Expr                    { EBinOp Add $1 $3 }
     | Expr '-' Expr                    { EBinOp Sub $1 $3 }
     | Expr '*' Expr                    { EBinOp Mul $1 $3 }
     | Expr '/' Expr                    { EBinOp Div $1 $3 }
     | Expr '%' Expr                    { EBinOp Mod $1 $3 }
     | Expr '==' Expr                   { EBinOp Eq $1 $3 }
     | Expr '!=' Expr                   { EBinOp Neq $1 $3 }
     | Expr '<' Expr                    { EBinOp Lt $1 $3 }
     | Expr '>' Expr                    { EBinOp Gt $1 $3 }
     | Expr '<=' Expr                   { EBinOp Le $1 $3 }
     | Expr '>=' Expr                   { EBinOp Ge $1 $3 }
     | Expr '&&' Expr                   { EBinOp And $1 $3 }
     | Expr '||' Expr                   { EBinOp Or $1 $3 }
     | '!' Expr                         { EUnOp Not $2 }
     | '-' Expr %prec NEG               { EUnOp Neg $2 }
     | if Expr then Expr else Expr      { EIf $2 $4 $6 }

Term : ident                            { EVar $1 }
     | number                           { ENum $1 }
     | string                           { EStr $1 }
     | true                             { EBool True }
     | false                            { EBool False }
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
