{
module Parser where
import AST
import Lexer
}

%name parser
%tokentype { Token }
%error { parseError }

%token
  tokIn       { TokIn }
  tokRecord   { TokRecord }
  tokEnum     { TokEnum }
  tokAction   { TokAction }
  tokNum      { TokNum }
  tokText     { TokText }
  tokArrow    { TokArrow }
  tokColon    { TokColon }
  tokComma    { TokComma }
  tokSemicolon { TokSemicolon }
  tokEq       { TokEq }
  tokPlus     { TokPlus }
  tokMinus    { TokMinus }
  tokStar     { TokStar }
  tokPipe     { TokPipe }
  tokLParen   { TokLParen }
  tokRParen   { TokRParen }
  tokIdent    { String } { TokIdent $$ }
  tokNumber   { Int }    { TokNumber $$ }
  tokString   { String } { TokString $$ }

%%

Program :: { Program }
  : tokIn tokIdent Stmts { Program $2 $3 }

Stmts :: { [Stmt] }
  : Stmt Stmts { $1 : $2 }
  |            { [] }

Stmt :: { Stmt }
  : tokNum tokIdent Params tokColon Expr { FunDecl $2 $3 TyNum $5 }
  | tokText tokIdent tokColon Expr        { VarDecl $2 TyText $4 }
  | tokAction tokIdent tokColon Stmts     { ActionDecl $2 $4 }
  | tokRecord tokIdent tokColon Fields    { RecordDecl $2 $4 }
  | tokEnum tokIdent tokColon EnumList    { EnumDecl $2 $4 }

Params :: { [(String, Type)] }
  : ParamList { $1 }
  |            { [] }

ParamList :: { [(String, Type)] }
  : Param { [$1] }
  | Param tokComma ParamList { $1 : $3 }

Param :: { (String, Type) }
  : tokNum tokIdent { ($2, TyNum) }
  | tokText tokIdent { ($2, TyText) }

Fields :: { [Field] }
  : Field { [$1] }
  | Field tokComma Fields { $1 : $3 }

Field :: { Field }
  : tokIdent tokType { Field $1 $2 }

EnumList :: { [String] }
  : tokIdent { [$1] }
  | tokIdent tokPipe EnumList { $1 : $3 }

Expr :: { Expr }
  : tokIdent           { EVar $1 }
  | tokNumber           { ENum $1 }
  | tokString           { EStr $1 }
  | Expr tokPlus Expr   { EBinOp "+" $1 $3 }
  | Expr tokMinus Expr  { EBinOp "-" $1 $3 }
  | Expr tokStar Expr   { EBinOp "*" $1 $3 }
  | tokIdent tokLParen Args tokRParen { ECall $1 $3 }

Args :: { [Expr] }
  : Expr { [$1] }
  | Expr tokComma Args { $1 : $3 }

%%

parseError :: [Token] -> a
parseError _ = error "Parse error"
