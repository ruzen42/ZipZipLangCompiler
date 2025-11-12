module Preprocessor (preprocess) where

import Text.Parsec
import Text.Parsec.String

preprocess :: String -> Either ParseError String
preprocess input = parse (preprocessor <* eof) "source" input

preprocessor :: Parser String
preprocessor = concat <$> many (try singleLineComment <|>
                               try multiLineComment <|>
                               try stringLiteral <|>
                               otherChar)
  where
    singleLineComment = do
      _ <- string "//"
      _ <- manyTill anyChar (try (char '\n') <|> (eof >> return '\n'))
      return "\n"

    multiLineComment = do
      _ <- string "/*"
      _ <- manyTill anyChar (try (string "*/"))
      return ""

    stringLiteral = do
      start <- char '"'
      content <- many (try (string "\\\"" >> return "\\\"") <|>
                      try (char '\\' >> anyChar >>= \c -> return ['\\', c]) <|>
                      (noneOf "\"" >>= \c -> return [c]))
      end <- char '"'
      return $ start : concat content ++ [end]

    otherChar = (:[]) <$> anyChar
