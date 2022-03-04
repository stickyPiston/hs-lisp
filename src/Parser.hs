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
  | Abst String Atom
  | Appl Atom Atom
  | Let String Atom Atom
  | Define Bool String Atom
  deriving Eq

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
  rest <- many (symbol <|> char '\'' <|> digit)
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
quote = Quote <$> (char '\'' *> (list <|> identifier <|> integer))

comment :: Parser Atom
comment = Comment <$> (char ';' >> manyTill (anyChar) (char '\n'))

curryAbst :: Atom -> [Atom] -> Atom
curryAbst expr is =
  foldl (flip $ Abst . extractIdentifier) (Abst (extractIdentifier $ last is) expr) (reverse $ init is)
  where
    extractIdentifier (Identifier n) = n

appl :: Parser Atom
appl = do
  l <- list
  case l of
    List (e1 : e2 : es) ->
      return $ if e1 `elem` keywords
         then List (e1 : e2 : es)
         else foldl Appl (Appl e1 e2) es
    _ -> fail "Not an application"
  where
    keywords = map Identifier ["if", "import"]

abst :: Parser Atom
abst = do
  l <- list
  case l of
    List [Identifier "lambda", List is, expr] ->
      return $ curryAbst expr is
    List [Identifier "λ", List is, expr] ->
      return $ curryAbst expr is
    _ -> fail "Not an abstraction"

lets :: Parser Atom
lets = do
  char '(' >> string "let" >> spaces >> char '('
  ((nm, val):bs) <- flip sepBy spaces $ do
    char '('
    (Identifier name) <- identifier
    spaces
    value <- atom
    char ')'
    return (name, value)
  char ')' >> spaces
  expr <- atom
  char ')'
  return $ foldl (\a (nm, val) -> Let nm val a) (Let nm val expr) bs

define :: Parser Atom
define = do
  char '('
  f <- try (string "define-rec") <|> string "define" 
  spaces
  let rec = if f == "define" then False else True
  r <- defunc rec <|> defvar rec
  char ')'
  return r
  where
    defunc rec = do
      char '('
      ((Identifier name) : is) <- identifier `sepBy` spaces
      char ')'
      spaces
      expr <- atom
      return $ Define rec name $ curryAbst expr is
    defvar rec = do
      (Identifier name) <- identifier
      spaces
      expr <- atom
      return $ Define rec name expr

atom :: Parser Atom
atom = integer
    <|> stringLiteral
    <|> identifier
    <|> quote
    <|> try define
    <|> try abst
    <|> try lets
    <|> try appl
    <|> list
    <|> comment

file :: Parser [Atom]
file = atom `endBy` (spaces <|> eof)

filterComments :: [Atom] -> [Atom]
filterComments = filter (not . isComment)
  where
    isComment (Comment _) = True
    isComment _           = False
