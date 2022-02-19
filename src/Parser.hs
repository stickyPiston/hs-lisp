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
  | Char Char

spaces :: Parser ()
spaces = skipMany1 space

symbol :: Parser Char
symbol = noneOf "\"\'()0123456789\n\t "

integer :: Parser Atom
integer = (Number . read) <$> (many1 digit)

char' :: Parser Char
char' = 
  (try $ do
    c <- char '\\' >> (oneOf "nt0rab'\\\"")
    return $ case c of
      'n' -> '\n'
      't' -> '\t'
      '0' -> '\0'
      'r' -> '\r'
      'a' -> '\a'
      'b' -> '\b'
      '\'' -> '\''
      '"' -> '"'
      '\\' -> '\\') <|> noneOf "\""

stringLiteral :: Parser Atom
stringLiteral = StringLiteral <$>
  (char '"' *> many char' <* char '"')

identifier :: Parser Atom
identifier = do
  first <- symbol
  rest <- many (symbol <|> char '\'')
  let atom = first : rest
   in return $
      case atom of
        "#f" -> Bool False
        "#t" -> Bool True
        "_" -> Wildcard
        _ -> Identifier atom

list :: Parser Atom
list = List <$> (char '(' *> skipMany space *> atom `sepBy` spaces <* skipMany space <* char ')')

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

filterComments :: [Atom] -> [Atom]
filterComments = filter (not . isComment)

isComment :: Atom -> Bool
isComment (Comment _) = True
isComment _           = False
