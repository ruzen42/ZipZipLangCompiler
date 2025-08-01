module Lexer (Token(..), lexingString) where

import Text.Parsec
import Text.Parsec.String 
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
  deriving (Eq)

instance Show Token where
  show (TKeyword s _) = "Keyword: " ++ s
  show (TIdentifier s _) = "Identifier: " ++ s
  show (TString s _) = "String: \"" ++ s ++ "\""
  show (TNumber n _) = "Number: " ++ show n
  show (TBoolean s _) = "Boolean: " ++ s
  show (TOperator s _) = "Operator: " ++ s
  show (TPunctuation c _) = "Punctuation: " ++ [c]
  show (TType s _) = "Type: " ++ s

keywords :: [String]
keywords = ["module", "main", "if", "then", "else", "use", "case", "zip"] 

typeNames :: [String]
typeNames = ["Num", "Logic", "Text", "ZippedText"]

lexer :: Parser [Token]
lexer = spaces *> many (tokenParser <* spaces) <* eof  

lexingString :: String -> Either ParseError [Token]
lexingString = parse lexer "lexer"

identifier :: Parser String
identifier = (:) <$> letter <*> many (alphaNum <|> char '_')

tokenParser :: Parser Token
tokenParser = choice
  [ try keywordParser
  , try typeParser    
  , try booleanParser  
  , try identifierParser
  , try stringParser    
  , try numberParser     
  , try operatorParser     
  , punctuationParser     
  ]

keywordParser :: Parser Token
keywordParser = do
  pos <- getPosition
  s <- try $ do
    s <- identifier
    guard (s `elem` keywords)
    notFollowedBy (alphaNum <|> char '_')  
    return s
  return $ TKeyword s pos

typeParser :: Parser Token
typeParser = do
  pos <- getPosition
  s <- try $ do
    s <- identifier 
    guard (s `elem` typeNames)
    notFollowedBy (alphaNum <|> char '_')  
    return s
  return $ TType s pos

booleanParser :: Parser Token
booleanParser = do
  pos <- getPosition
  s <- try $ do
    s <- string "True" <|> string "False"
    notFollowedBy (alphaNum <|> char '_')  
    return s
  return $ TBoolean s pos

identifierParser :: Parser Token
identifierParser = do
  pos <- getPosition
  s <- identifier
  return $ TIdentifier s pos

stringParser :: Parser Token
stringParser = do
  pos <- getPosition
  _ <- char '"'
  s <- many (try (string "\\\"" >> return '"') <|>  
            try (char '\\' >> anyChar >>= \c -> return c) <|>  
            noneOf "\"")  
  _ <- char '"'
  return $ TString s pos

numberParser :: Parser Token
numberParser = do
  pos <- getPosition
  digits <- many1 digit
  frac <- option "" (liftM2 (:) (char '.') (many1 digit))
  let numStr = digits ++ frac
  return $ TNumber (read numStr) pos

operatorParser :: Parser Token
operatorParser = do
  pos <- getPosition
  op <- choice (map (try . string) ["++", "==", "::", "&&", "?", "->", "+", "*", "/", "$", "@"])
  return $ TOperator op pos

punctuationParser :: Parser Token
punctuationParser = do
  pos <- getPosition
  c <- oneOf "(){},=:."
  return $ TPunctuation c pos
