module Parser
  ( readScheme,
    getParser,
    car,
    cdr,
    SchemeValue,
  )
where

import Control.Applicative (Alternative (empty, many, (<|>)))
import Data.Char (isDigit, isSpace)
import Data.Monoid (Any (Any, getAny))
import Data.Tuple (swap)

data SchemeValue
  = SchemeEmpty
  | SchemeBool Bool
  | SchemeVar String
  | SchemeNum Integer -- NOTE: No floats yet
  | SchemeString String -- NOTE: no escaping support
  | SchemeQuote SchemeValue
  | SchemeCons {car :: SchemeValue, cdr :: SchemeValue}
  deriving (Show, Eq)

newtype Parser a = Parser {getParser :: String -> Maybe (String, a)}

-- instances
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

-- Helpers
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
spanP p = Parser $ Just . swap . span p

notAllowed :: Char -> Bool
notAllowed = not . getAny . foldMap (Any .) [isSpace, (== '\''), (== ')'), (== '(')]

ws :: Parser String
ws = spanP isSpace

notNull :: Parser [a] -> Parser [a]
notNull (Parser p) =
  Parser $ \input -> do
    (input', as) <- p input
    if null as
      then Nothing
      else Just (input', as)

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy element seperator = (:) <$> element <*> many (seperator *> element) <|> pure []

listToCons :: [SchemeValue] -> SchemeValue
listToCons = foldr SchemeCons SchemeEmpty

-- parsers
schemeEmpty :: Parser SchemeValue
schemeEmpty = SchemeEmpty <$ (charP '(' *> ws <* charP ')')

schemeBool :: Parser SchemeValue
schemeBool = f <$> (stringP "#t" <|> stringP "#f")
  where
    f "#t" = SchemeBool True
    f "#f" = SchemeBool False
    f _ = error "this should not happen"

schemeQuote :: Parser SchemeValue
schemeQuote = SchemeQuote <$> (charP '\'' *> schemeValue)

schemeNum :: Parser SchemeValue
schemeNum = SchemeNum . read <$> notNull (spanP isDigit)

schemeString :: Parser SchemeValue
schemeString = SchemeString <$> (charP '"' *> spanP (/= '"') <* charP '"')

schemeVar :: Parser SchemeValue
schemeVar = SchemeVar <$> notNull (ws *> spanP notAllowed <* ws)

schemeCons :: Parser SchemeValue
schemeCons = listToCons <$> (charP '(' *> ws *> cons <* ws <* charP ')')
  where
    cons = sepBy schemeValue ws

-- schemeSexp :: Parser SchemeValue
-- schemeSexp = SchemeSexp <$> (charP '(' *> ws *> call <* ws <* charP ')')
--   where
--     call = (\(SchemeVar hd) rest -> (hd, rest)) <$> schemeVar <*> (ws *> sepBy schemeValue ws)

-- -- Final value
schemeValue :: Parser SchemeValue
schemeValue =
  schemeQuote
    <|> schemeEmpty
    <|> schemeBool
    <|> schemeNum
    <|> schemeString
    <|> schemeVar
    <|> schemeCons

readScheme :: Parser SchemeValue
readScheme = listToCons <$> many (ws *> schemeValue <* ws)
