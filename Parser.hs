module Parser where

import Control.Applicative
import Data.Char

newtype Parser a =
  Parser
    { parse :: String -> Maybe (String, a)
    }

instance Functor Parser where
  fmap f (Parser p) =
    Parser $ \input -> do
      (r, t) <- p input
      return (r, f t)

instance Applicative Parser where
  pure a = Parser $ \input -> return (input, a)
  (Parser p1) <*> (Parser p2) =
    Parser $ \input -> do
      (r, t) <- p1 input
      (r', t') <- p2 r
      return $ (r', t t')

instance Alternative Parser where
  empty = Parser $ \_ -> Nothing
  (Parser p1) <|> (Parser p2) =
    Parser $ \input -> p1 input <|> p2 input

charP :: Char -> Parser Char
charP x = Parser f
  where
    f (c:cs)
      | c == x = Just (cs, c)
      | otherwise = Nothing
    f _ = Nothing

stringP :: String -> Parser String
stringP x = traverse charP x

spansP :: (Char -> Bool) -> Parser String
spansP = some . parseIf

spanmP :: (Char -> Bool) -> Parser String
spanmP = many . parseIf

parseIf :: (Char -> Bool) -> Parser Char
parseIf f = Parser $ ca
  where
    ca (c:cs) =
      case f c of
        True -> Just (cs, c)
        False -> Nothing
    ca [] = Nothing

-- Lisp specific grammar

data Atom
  = Number Int
  | Identifier String
  | StringLiteral String
  | List [Atom]
  | Quote Atom

integer :: Parser Atom
integer = Number . read <$> spansP isDigit

identifier :: Parser Atom
identifier =
  Identifier <$>
  (spansP $ \c ->
     any ($ c) [isAlpha, isNumber, isPunctuation, isSymbol] &&
     c `notElem` ['(', ')', '\'', '"', ';'])

stringliteral :: Parser Atom
stringliteral =
  StringLiteral <$>
  (charP '"' *> spanmP (/= '"') <* charP '"')

wsm :: Parser String
wsm = spanmP isSpace

wss :: Parser String
wss = spansP isSpace

list :: Parser Atom
list =
  List <$> (charP '(' *> wsm *> body <* wsm <* charP ')')
  where
    body = (:) <$> atom <*> many (wss *> atom) <|> pure []

quote :: Parser Atom
quote = Quote <$> (charP '\'' *> atom)

comment :: Parser Atom
comment = charP ';' *> spanmP (/= '\n') *> wss *> atom

atom :: Parser Atom
atom =
  integer <|> identifier <|> stringliteral <|> list <|>
  quote <|> comment

file :: Parser [Atom]
file = many $ wsm *> atom
