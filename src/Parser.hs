module Parser where

import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces)
import Text.Parsec.Prim hiding (try)

data Atom
  = Number Int
  | Identifier String
  | StringLiteral String
  | List [Atom]
  | DottedList [Atom] Atom
  | Quote Atom
  | Bool Bool
data ParsedAtom = ParsedAtom
  { pos :: SourcePos, atm :: Atom }

sourcePos :: Monad m => ParsecT s u m SourcePos
sourcePos = statePos `liftM` getParserState

spaces :: Parser ()
spaces = skipMany1 space

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~,."

integer :: Parser ParsedAtom
integer = do
  pos <- sourcePos
  n <- many1 digit
  return $ ParsedAtom pos $ Number . read $ n

stringLiteral :: Parser ParsedAtom
stringLiteral = do
  pos <- sourcePos
  char '"'
  x <- many (noneOf "\"")
  char '"'
  return $ ParsedAtom pos $ StringLiteral x

identifier :: Parser ParsedAtom
identifier = do
  pos <- sourcePos
  first <- letter <|> symbol
  rest <- many (letter <|> symbol <|> digit <|> char '\'')
  let atom = first : rest
   in return $ ParsedAtom pos $
      case atom of
        "#f" -> Bool False
        "#t" -> Bool True
        _ -> Identifier atom

list :: Parser ParsedAtom
list = do
  pos <- sourcePos
  as <- atom `sepBy` spaces
  return $ ParsedAtom pos $ List (map atm as)

dottedList :: Parser ParsedAtom
dottedList = do
  pos <- sourcePos
  h <- atom `endBy` spaces
  t <- char '.' >> spaces >> atom
  return $ ParsedAtom pos $ DottedList (map atm h) (atm t)

quote :: Parser ParsedAtom
quote = do
  pos <- sourcePos
  char '\''
  a <- atom
  return $ ParsedAtom pos $ Quote (atm a)

comment :: Parser ParsedAtom
comment =
  char ';' >> many (noneOf "\n") >> spaces >> atom

atom :: Parser ParsedAtom
atom = integer
    <|> stringLiteral
    <|> identifier
    <|> quote
    <|> comment
    <|> do
      char '('
      l <- try list <|> dottedList
      char ')'
      return l

file :: Parser [ParsedAtom]
file = atom `endBy` spaces