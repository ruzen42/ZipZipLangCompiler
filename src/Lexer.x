{
module Lexer where

import Data.Char (isAlpha, isAlphaNum)
}

%wrapper "basic"

$digit   = 0-9
$alpha   = [A-Za-z]
$idchar  = [A-Za-z0-9_]
$space   = [\ \t\n\r]
$stringchar = [^\"\\]

tokens :-

  $space+                             ;
  "--".*                              ;

  "in"                                { \_ -> TokIn }
  "record"                            { \_ -> TokRecord }
  "enum"                              { \_ -> TokEnum }
  "if"                                { \_ -> TokIf }
  "then"                              { \_ -> TokThen }
  "else"                              { \_ -> TokElse }
  "let"                               { \_ -> TokLet }
  "True"                              { \_ -> TokTrue }
  "False"                             { \_ -> TokFalse }

  "Action"                            { \_ -> TokAction }
  "Num"                               { \_ -> TokNum }
  "Text"                              { \_ -> TokText }
  "Logic"                             { \_ -> TokLogic }

  ":"                                 { \_ -> TokColon }
  ","                                 { \_ -> TokComma }
  ";"                                 { \_ -> TokSemicolon }
  "="                                 { \_ -> TokEq }
  "=="                                { \_ -> TokEqEq }
  "!="                                { \_ -> TokNeq }
  "<"                                 { \_ -> TokLt }
  ">"                                 { \_ -> TokGt }
  "<="                                { \_ -> TokLe }
  ">="                                { \_ -> TokGe }
  "->"                                { \_ -> TokArrow }
  "+"                                 { \_ -> TokPlus }
  "-"                                 { \_ -> TokMinus }
  "*"                                 { \_ -> TokStar }
  "/"                                 { \_ -> TokSlash }
  "%"                                 { \_ -> TokPercent }
  "&&"                                { \_ -> TokAnd }
  "||"                                { \_ -> TokOr }
  "!"                                 { \_ -> TokNot }
  "|"                                 { \_ -> TokPipe }
  "("                                 { \_ -> TokLParen }
  ")"                                 { \_ -> TokRParen }
  "{"                                 { \_ -> TokLBrace }
  "}"                                 { \_ -> TokRBrace }

  \"($stringchar|\\.)*\"              { \s -> TokString (read s) }

  $digit+                             { \s -> TokNumber (read s) }

  $alpha$idchar*                      { \s -> TokIdent s }

  .                                   { \s -> error $ "Unexpected character: " ++ s }

{

data Token
  = TokIn
  | TokRecord
  | TokEnum
  | TokIf
  | TokThen
  | TokElse
  | TokLet
  | TokTrue
  | TokFalse
  | TokAction
  | TokNum
  | TokText
  | TokLogic
  | TokArrow
  | TokColon
  | TokComma
  | TokSemicolon
  | TokEq
  | TokEqEq
  | TokNeq
  | TokLt
  | TokGt
  | TokLe
  | TokGe
  | TokPlus
  | TokMinus
  | TokStar
  | TokSlash
  | TokPercent
  | TokAnd
  | TokOr
  | TokNot
  | TokPipe
  | TokLParen
  | TokRParen
  | TokLBrace
  | TokRBrace
  | TokIdent String
  | TokNumber Int
  | TokString String
  deriving (Show, Eq)

}

