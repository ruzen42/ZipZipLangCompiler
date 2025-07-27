module Lexer (Token(..), lexer) where

import Text.Parsec
import Text.Parsec.String 
import Text.Parsec.Char
import Control.Monad 

data Token
  = TKeyword String SourcePos
  | TIdentifier String SourcePos
  | TString String SourcePos
  | TNumber Integer SourcePos
  | TBoolean String SourcePos
  | TOperator String SourcePos
  | TPunctuation Char SourcePos
  | TType String SourcePos
  deriving (Show, Eq)

keywords :: [String]
keywords = ["name", "main", "print", "if", "then", "else"] 

typeNames :: [String]
typeNames = ["Num", "Logic", "Text"]

lexer :: Parser [Token]
lexer = whiteSpace *> many (tokenParser <* whiteSpace) <* eof

identifier :: Parser String
identifier = (:) <$> letter <*> many (alphaNum <|> char '_')

whiteSpace :: Parser ()
whiteSpace = void $ many (simpleWhitespace <|> singleLineComment <|> multiLineComment)
  where
    simpleWhitespace = void $ oneOf " \t\n\r"
    singleLineComment = string "//" *> skipMany (noneOf "\n") <* char '\n'
    multiLineComment = void $ string "/*" *> skipMultiLineComment
    skipMultiLineComment = try (string "*/") <|> (anyChar *> skipMultiLineComment)

tokenParser :: Parser Token
tokenParser = choice
  [ keywordParser
  , typeParser
  , booleanParser
  , identifierParser
  , stringParser
  , numberParser
  , operatorParser
  , punctuationParser
  ]

keywordParser :: Parser Token
keywordParser = do
  pos <- getPosition
  s <- try $ do
    s <- identifier
    guard (s `elem` keywords)
    return s
  return $ TKeyword s pos

typeParser :: Parser Token
typeParser = do
  pos <- getPosition
  s <- try $ do
    s <- identifier 
    guard (s `elem` typeNames)
    return s
  return $ TType s pos

booleanParser :: Parser Token
booleanParser = do
  pos <- getPosition
  s <- string "True" <|> string "False"
  return $ TBoolean s pos

identifierParser :: Parser Token
identifierParser = do
  pos <- getPosition
  s <- identifier
  if s `elem` keywords || s `elem` typeNames
    then fail "Keyword or type used as identifier"
    else return $ TIdentifier s pos
  where
    identifier = (:) <$> letter <*> many (alphaNum <|> char '_')

stringParser :: Parser Token
stringParser = do
  pos <- getPosition
  char '"'
  s <- many (noneOf "\"" <|> (char '\\' *> char '"'))
  char '"'
  return $ TString s pos

numberParser :: Parser Token
numberParser = do
  pos <- getPosition
  digits <- many1 digit
  return $ TNumber (read digits) pos

operatorParser :: Parser Token
operatorParser = do
  pos <- getPosition
  op <- choice (map string ["++", "+", "*", "==", "->"])
  return $ TOperator op pos

punctuationParser :: Parser Token
punctuationParser = do
  pos <- getPosition
  c <- oneOf "(){},=:"
  return $ TPunctuation c pos

