module Parser where

import System.IO.Unsafe
import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces)
import Text.Parsec.Prim hiding (try)

data Statement
  = Import String String
  | Define Bool String Expression
  deriving Eq

data Expression
  = Number Int
  | Identifier String
  | StringLiteral String
  | List [Expression]
  | Quote Expression
  | Bool Bool
  | Wildcard
  | Abst String Expression
  | Appl Expression Expression
  | Let String Expression Expression
  | Comment String
  | If Expression Expression Expression
  deriving Eq

instance Show Expression where
  show (Parser.List as) = "(" ++ unwords (map show as) ++ ")"
  show (Parser.Number n) = show n
  show (Identifier i) = i
  show (StringLiteral s) = "\"" ++ s ++ "\""
  show (Quote a) = "'" ++ show a
  show (Parser.Bool True) = "#t"
  show (Parser.Bool False) = "#f"
  show Wildcard = "_"
  show (Abst p b) = "(λ (" ++ p ++ ") " ++ show b ++ ")" 
  show (Appl f a) = "(" ++ show f ++ " " ++ show a ++ ")"
  show (If c t e) = "(if " ++ show c ++ " " ++ show t ++ " " ++ show e ++ ")"

instance Show Statement where
  show (Define False n t) = "(define " ++ n ++ " " ++ show t ++ ")"  
  show (Define True n t) = "(define-rec " ++ n ++ " " ++ show t ++ ")"  
  show (Import "" path) = "(import " ++ path ++ ")"
  show (Import as path) = "(import-as " ++ as ++ " " ++ path ++ ")"

{-
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
  deriving Eq-}

spaces :: Parser ()
spaces = skipMany1 space

symbol :: Parser Char
symbol = noneOf "\"\'()0123456789\n\t "

integer :: Parser Expression
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

stringLiteral :: Parser Expression
stringLiteral = StringLiteral <$>
  (char '"' *> many char' <* char '"')

identifier :: Parser Expression
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

list :: Parser Expression
list = List <$> (char '(' *> expr `sepBy` spaces <* char ')')

quote :: Parser Expression
quote = Quote <$> (char '\'' *> (list <|> identifier <|> integer))

comment :: Parser Expression
comment = Comment <$> (char ';' >> manyTill (anyChar) (char '\n'))

curryAbst :: Expression -> [Expression] -> Expression
curryAbst expr is =
  foldl (flip $ Abst . extractIdentifier) (Abst (extractIdentifier $ last is) expr) (reverse $ init is)
  where
    extractIdentifier (Identifier n) = n

appl :: Parser Expression
appl = do
  l <- list
  case l of
    List (e1 : e2 : es)
      | e1 `elem` keywords -> fail "Not an application"
      | otherwise -> return $ foldl Appl (Appl e1 e2) es
    _ -> fail "Not an application"
  where
    keywords = map Identifier ["lambda", "λ", "import", "define", "define-rec"]

abst :: Parser Expression
abst = do
  l <- list
  case l of
    List [Identifier "lambda", List is, value] ->
      return $ curryAbst value is
    List [Identifier "λ", List is, value] ->
      return $ curryAbst value is
    _ -> fail "Not an abstraction"

lets :: Parser Expression
lets = do
  char '(' >> string "let" >> spaces >> char '('
  ((nm, val):bs) <- flip sepBy spaces $ do
    char '('
    (Identifier name) <- identifier
    spaces
    value <- expr
    char ')'
    return (name, value)
  char ')' >> spaces
  value <- expr
  char ')'
  return $ foldl (\a (nm, val) -> Let nm val a) (Let nm val value) bs

define :: Parser Statement
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
      expr <- expr
      return $ Define rec name $ curryAbst expr is
    defvar rec = do
      (Identifier name) <- identifier
      spaces
      expr <- expr
      return $ Define rec name expr

imports :: Parser Statement
imports = do
  try import_path <|> import_as
  where
    import_path = do
      char '(' >> string "import" >> spaces
      (StringLiteral path) <- stringLiteral
      char ')'
      return $ Import "" path
    import_as = do
      char '(' *> string "import-as" *> spaces
      (Identifier name) <- identifier <* spaces
      (StringLiteral path) <- stringLiteral <* char ')'
      return $ Import name path
ifs :: Parser Expression
ifs = do
  char '(' >> string "if" >> spaces
  cond <- expr
  spaces
  thens <- expr
  spaces
  elses <- expr
  char ')'
  return $ If cond thens elses

expr :: Parser Expression
expr = integer
    <|> stringLiteral
    <|> identifier
    <|> quote
    <|> try ifs
    <|> try abst
    <|> try lets
    <|> try appl
    <|> list
    <|> comment

stmt :: Parser Statement
stmt = try define <|> imports

file :: Parser [Statement]
file = stmt `endBy` (spaces <|> eof)
