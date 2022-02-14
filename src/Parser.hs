module Parser where

import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces)
import Text.Parsec.Prim hiding (try)

data Atom
  = Number Int
  | Identifier String
  | StringLiteral String
  | List [Atom]
  | Quote Atom
  | Bool Bool
  | Wildcard
  | Comment String

spaces :: Parser ()
spaces = skipMany1 space

symbol :: Parser Char
symbol = noneOf "\"\'()0123456789\n\t "

integer :: Parser Atom
integer = (Number . read) <$> (many1 digit)

stringLiteral :: Parser Atom
stringLiteral = StringLiteral <$> (char '"' *> many (noneOf "\"") <* char '"')

identifier :: Parser Atom
identifier = do
  first <- letter <|> symbol
  rest <- many (letter <|> symbol <|> digit <|> char '\'')
  let atom = first : rest
   in return $
      case atom of
        "#f" -> Bool False
        "#t" -> Bool True
        "_" -> Wildcard
        _ -> Identifier atom

list :: Parser Atom
list = List <$> (char '(' *> atom `sepBy` spaces <* char ')')

quote :: Parser Atom
quote = Quote <$> (char '\'' *> atom)

comment :: Parser Atom
comment = Comment <$> (char ';' >> manyTill (anyChar) (char '\n'))

atom :: Parser Atom
atom = integer
    <|> stringLiteral
    <|> identifier
    <|> quote
    <|> list
    <|> comment

file :: Parser [Atom]
file = atom `endBy` (spaces <|> eof)
