{-# LANGUAGE OverloadedStrings #-}
module Parse (parse) where

import Data.Void (Void)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import Control.Monad (void)

import VM

import Text.Megaparsec (Parsec, (<|>), many, eof, sepEndBy, try)
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

parse :: Text -> Either String (Vector Instruction)
parse input =
  case M.parse program "" input of
    Left err     -> Left $ M.errorBundlePretty err
    Right instrs -> Right $ V.snoc instrs Halt

program :: Parser (Vector Instruction)
program = do
  sc
  instrs <- statement `sepEndBy` newlines
  eof
  pure (V.fromList instrs)

sc :: Parser ()
sc = L.space (void $ M.takeWhile1P Nothing (`elem` (" \t" :: String)))
             (L.skipBlockComment "/*" "*/")
             M.empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

newlines :: Parser ()
newlines = void $ M.some $ lexeme (void C.eol)

statement :: Parser Instruction
statement = try actionWithArguments <|> actionWithoutArguments <|> assignment

actionWithArguments :: Parser Instruction
actionWithArguments = do
  actionName <- identifier 
  _          <- symbol "("
  arg        <- argument 
  _          <- symbol ")"
  pure $ case actionName of
    "print"   -> Print arg 
    _         -> error "undefined function"

actionWithoutArguments :: Parser Instruction
actionWithoutArguments = do
  actionName <- identifier 
  pure $ case actionName of
    "halt"    -> Halt 
    _         -> error "undefined function"


argument :: Parser Arg
argument = 
  try (ArgLit <$> valueWithType)
  <|> (ArgVar <$> identifier)

assignment :: Parser Instruction
assignment = do
  name  <- identifier
  _     <- symbol "="
  value <- valueWithType
  pure $ SetVar name value

identifier :: Parser Text
identifier = lexeme $ do
  first <- C.letterChar <|> C.char '_'
  rest  <- many (C.alphaNumChar <|> C.char '_')
  pure $ T.pack $ first : rest

valueWithType :: Parser Value
valueWithType =
  try (numLiteral  <* colon <* symbol "Num")
  <|> (textLiteral <* colon <* symbol "Text")
  <|> (logicLiteral <* colon <* symbol "Logic")

colon :: Parser Text
colon = symbol ":"

numLiteral :: Parser Value
numLiteral = lexeme $ do
  sign   <- M.option "" (T.singleton <$> C.char '-')
  digits <- M.some C.digitChar
  pure $ VNum $ read $ T.unpack sign ++ digits

logicLiteral :: Parser Value
logicLiteral = lexeme $ 
  (VLogic True <$ symbol "Yes") <|> (VLogic False <$ symbol "No") 

textLiteral :: Parser Value
textLiteral = lexeme $ do
  _    <- C.char '"'
  body <- many stringChar
  _    <- C.char '"'
  pure (VText (T.pack body))
  where
    stringChar =
      (C.char '\\' *> escapedChar) <|> M.satisfy (\c -> c /= '"' && c /= '\\')

    escapedChar =
      (C.char '"'  >> pure '"')  <|>
      (C.char '\\' >> pure '\\') <|>
      (C.char 'n'  >> pure '\n') <|>
      (C.char 't'  >> pure '\t')
