module Parser where

import Control.Applicative
import Data.Char

data SchemeValue
  = SchemeBool Bool
  | SchemeSym String -- This is different but I am not sure how to store it.
  | SchemeNum Integer
  | SchemeString String
  | SchemeList [SchemeValue]
  | SchemeSexp (String, [SchemeValue])
  deriving (Show, Eq)

newtype Parser a = Parser {getParser :: String -> Maybe (String, a)}

instance Functor Parser where
  fmap f (Parser p) = Parser $ \input -> do
    (input', a) <- p input
    return (input', f a)

instance Applicative Parser where
  pure a = Parser $ \input -> Just (input, a)

  (Parser one) <*> (Parser two) = Parser $
    \input -> do
      (input', f) <- one input
      (input'', a) <- two input'
      return (input'', f a)

instance Alternative Parser where
  empty = Parser $ const Nothing

  (Parser one) <|> (Parser two) = Parser $ \input -> one input <|> two input

charP :: Char -> Parser Char
charP char = Parser f
  where
    f (c : rest)
      | c == char = Just (rest, c)
      | otherwise = Nothing
    f [] = Nothing

stringP :: [Char] -> Parser [Char]
stringP = traverse charP

spanP :: (Char -> Bool) -> Parser String
spanP p = Parser $ \input ->
  let (token, rest) = span p input
   in return (rest, token)

schemeBool :: Parser SchemeValue
schemeBool = f <$> (stringP "#t" <|> stringP "#f")
  where
    f "#t" = SchemeBool True
    f "#f" = SchemeBool False
    f _ = error "this should not happen"

schemeSym :: Parser SchemeValue
schemeSym = SchemeSym <$> (charP '\'' *> spanP (\c -> not (isSpace c || c == '\'')))

schemeNum :: Parser SchemeValue
schemeNum = undefined

schemeValue :: Parser SchemeValue
schemeValue = schemeBool <|> schemeSym
