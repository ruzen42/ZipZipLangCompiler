module Parser (parseProgram, parseExpr) where

import Text.Parsec hiding (token)

import Lexer (Token(..), lexingString)
import AST

type TokenParser = Parsec [Token] ()

token :: (Token -> Maybe a) -> TokenParser a
token test = tokenPrim show updatePos test
  where
    updatePos :: SourcePos -> Token -> [Token] -> SourcePos
    updatePos pos _ _ = pos

keyword :: String -> TokenParser String
keyword kw = token $ \t -> case t of
  TKeyword s _ | s == kw -> Just s
  _ -> Nothing

identifier :: TokenParser String
identifier = token $ \t -> case t of
  TIdentifier s _ -> Just s
  _ -> Nothing

stringLit :: TokenParser String
stringLit = token $ \t -> case t of
  TString s _ -> Just s
  _ -> Nothing

numberLit :: TokenParser Integer
numberLit = token $ \t -> case t of
  TNumber n _ -> Just n
  _ -> Nothing

booleanLit :: TokenParser Bool
booleanLit = token $ \t -> case t of
  TBoolean "True" _ -> Just True
  TBoolean "False" _ -> Just False
  _ -> Nothing

typeName :: TokenParser String
typeName = token $ \t -> case t of
  TType s _ -> Just s
  _ -> Nothing

operator :: String -> TokenParser String
operator op = token $ \t -> case t of
  TOperator s _ | s == op -> Just s
  _ -> Nothing

punctuation :: Char -> TokenParser Char
punctuation p = token $ \t -> case t of
  TPunctuation c _ | c == p -> Just c
  _ -> Nothing

parseProgram :: [Token] -> Either ParseError Program
parseProgram tk = parse programParser "parser" tk 

programParser :: TokenParser Program
programParser = do
  _ <- keyword "module"
  moduleName <- identifier
  definitions <- many definitionParser
  eof
  return $ PProgram moduleName definitions

definitionParser :: TokenParser Definition
definitionParser = choice
  [ try functionDefParser
  , mainBlockParser
  ]

functionDefParser :: TokenParser Definition
functionDefParser = try $ do
  funcName <- identifier
  _ <- operator "::"
  functionType <- parseFunctionTypeSignature
  _ <- punctuation '='
  body <- exprParser
  let (paramTypes, returnType) = functionType
  let paramNames = ["x" ++ show i | i <- [1..length paramTypes]]
  let params = zip paramNames paramTypes
  return $ DFunctionDef funcName params returnType body
  where
    parseFunctionTypeSignature = do
      types <- sepBy1 typeName (operator "->")
      case reverse types of
        (ret:params) -> return (reverse params, ret)
        [] -> return ([], "()")

mainBlockParser :: TokenParser Definition
mainBlockParser = do
  _ <- keyword "main"
  _ <- punctuation '='
  body <- exprParser
  return $ DMainBlock body

exprParser :: TokenParser Expr
exprParser = ifExprParser

ifExprParser :: TokenParser Expr
ifExprParser = choice
  [ do
      _ <- keyword "if"
      cond <- orExprParser
      _ <- keyword "then"
      thenExpr <- orExprParser
      _ <- keyword "else"
      elseExpr <- orExprParser
      return $ EIf cond thenExpr elseExpr
  , orExprParser
  ]

orExprParser :: TokenParser Expr
orExprParser = do
  left <- andExprParser
  rest <- many $ do
    op <- operator "||"
    right <- andExprParser
    return (op, right)
  return $ foldl (\acc (op, r) -> EBinaryOp op acc r) left rest

andExprParser :: TokenParser Expr
andExprParser = do
  left <- equalityExprParser
  rest <- many $ do
    op <- operator "&&"
    right <- equalityExprParser
    return (op, right)
  return $ foldl (\acc (op, r) -> EBinaryOp op acc r) left rest

equalityExprParser :: TokenParser Expr
equalityExprParser = do
  left <- additiveExprParser
  rest <- many $ do
    op <- operator "=="
    right <- additiveExprParser
    return (op, right)
  return $ foldl (\acc (op, r) -> EBinaryOp op acc r) left rest

additiveExprParser :: TokenParser Expr
additiveExprParser = do
  left <- multiplicativeExprParser
  rest <- many $ do
    op <- choice [operator "+", operator "++"]
    right <- multiplicativeExprParser
    return (op, right)
  return $ foldl (\acc (op, r) -> EBinaryOp op acc r) left rest

multiplicativeExprParser :: TokenParser Expr
multiplicativeExprParser = do
  left <- primaryExprParser
  rest <- many $ do
    op <- choice [operator "*", operator "/"]
    right <- primaryExprParser
    return (op, right)
  return $ foldl (\acc (op, r) -> EBinaryOp op acc r) left rest

primaryExprParser :: TokenParser Expr
primaryExprParser = choice
  [ numberExprParser
  , stringExprParser
  , booleanExprParser
  , functionCallParser
  , identifierExprParser
  , parenthesizedExprParser
  , blockExprParser
  ]

numberExprParser :: TokenParser Expr
numberExprParser = ENumber <$> numberLit

stringExprParser :: TokenParser Expr
stringExprParser = EString <$> stringLit

booleanExprParser :: TokenParser Expr
booleanExprParser = EBool <$> booleanLit

identifierExprParser :: TokenParser Expr
identifierExprParser = EIdentifier <$> identifier

functionCallParser :: TokenParser Expr
functionCallParser = try $ do
  funcName <- identifier
  _ <- punctuation '('
  args <- sepBy exprParser (punctuation ',')
  _ <- punctuation ')'
  return $ EFunctionCall funcName args

parenthesizedExprParser :: TokenParser Expr
parenthesizedExprParser = do
  _ <- punctuation '('
  expr <- exprParser
  _ <- punctuation ')'
  return expr

blockExprParser :: TokenParser Expr
blockExprParser = do
  _ <- punctuation '{'
  exprs <- sepBy exprParser (punctuation ',')
  _ <- punctuation '}'
  return $ EBlock exprs

parseExpr :: String -> Either ParseError Expr
parseExpr input = do
  tk <- lexingString input
  parse exprParser "expression" tk 
