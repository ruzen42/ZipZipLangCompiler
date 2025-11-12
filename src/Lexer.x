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
  "Action"                            { \_ -> TokAction }
  "Num"                               { \_ -> TokNum }
  "Text"                              { \_ -> TokText }

  ":"                                 { \_ -> TokColon }
  ","                                 { \_ -> TokComma }
  ";"                                 { \_ -> TokSemicolon }
  "="                                 { \_ -> TokEq }
  "->"                                { \_ -> TokArrow }
  "+"                                 { \_ -> TokPlus }
  "-"                                 { \_ -> TokMinus }
  "*"                                 { \_ -> TokStar }
  "|"                                 { \_ -> TokPipe }
  "("                                 { \_ -> TokLParen }
  ")"                                 { \_ -> TokRParen }

  \"($stringchar|\\.)*\"              { \s -> TokString (read s) }

  $digit+                             { \s -> TokNumber (read s) }

  $alpha$idchar*                      { \s -> TokIdent s }

  .                                   { \s -> error $ "Unexpected character: " ++ s }

{

data Token
  = TokIn
  | TokRecord
  | TokEnum
  | TokAction
  | TokNum
  | TokText
  | TokArrow
  | TokColon
  | TokComma
  | TokSemicolon
  | TokEq
  | TokPlus
  | TokMinus
  | TokStar
  | TokPipe
  | TokLParen
  | TokRParen
  | TokIdent String
  | TokNumber Int
  | TokString String
  deriving (Show, Eq)

}
