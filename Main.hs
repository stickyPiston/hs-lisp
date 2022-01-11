import Control.Applicative
import Data.Char

import System.Environment
import qualified Data.Map.Strict as Map

newtype Parser a = Parser { parse :: String -> Maybe (String, a) }

instance Functor Parser where
  fmap f (Parser p) = Parser $ \input -> do
    (r, t) <- p input
    return (r, f t)

instance Applicative Parser where
  pure a = Parser $ \input -> return (input, a)
  (Parser p1) <*> (Parser p2) = Parser $ \input -> do
    (r, t) <- p1 input
    (r', t') <- p2 r
    return $ (r', t t')

instance Alternative Parser where
  empty = Parser $ \_ -> Nothing
  (Parser p1) <|> (Parser p2) =
    Parser $ \input -> p1 input <|> p2 input

charP :: Char -> Parser Char
charP x = Parser f
  where f (c : cs)
          | c == x    = Just (cs, c)
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
  where ca (c : cs) = 
          case f c of
            True  -> Just (cs, c)
            False -> Nothing
        ca [] = Nothing

data Atom = Number Int | Identifier String | StringLiteral String | List [Atom] | Quote Atom
  deriving (Show, Eq)

integer :: Parser Atom
integer = Number . read <$> spansP isDigit

identifier :: Parser Atom
identifier = Identifier <$> (spansP $
  \c -> any ($ c) [isAlpha, isNumber, isPunctuation, isSymbol]
    && c /= '(' && c /= ')' && c /= '\'' && c /= '"' && c /= ';')

stringliteral :: Parser Atom
stringliteral = StringLiteral <$>
  (charP '"' *> spanmP (/= '"') <* charP '"')

wsm :: Parser String
wsm = spanmP isSpace

wss :: Parser String
wss = spansP isSpace

list :: Parser Atom
list = List <$> (charP '(' *> wsm *> body <* wsm <* charP ')')
  where body = (:) <$> atom <*> many (wss *> atom) <|> pure []

quote :: Parser Atom
quote = Quote <$> (charP '\'' *> atom)

comment :: Parser Atom
comment = charP ';' *> spanmP (/= '\n') *> wss *> atom

atom :: Parser Atom
atom = integer <|> identifier <|> stringliteral <|> list <|> quote <|> comment

file :: Parser [Atom]
file = many $ wsm *> atom

type Scope = Map.Map String RuntimeValue
data RuntimeValue = Number' Int | String' String | List' [RuntimeValue] | IOAction' RuntimeValue | Lambda' [String] Atom | Nil deriving (Eq)
instance Show RuntimeValue where
  show (Number'   a) = show a
  show (String'   a) = a
  show (List'     a) = "("
    ++ (show . head $ a)
    ++ (foldl (\a e -> a ++ " " ++ show e) "" $ tail a)
    ++ ")"
  show (IOAction' a) = show a
  show Nil           = "Nil" 
  show (Lambda' _ _) = "Lambda"

(Number' a) + (Number' b) = Number' $ (Prelude.+) a b
(String' a) + (String' b) = String' $ a ++ b
(List'   a) + (List'   b) = List'   $ a ++ b
a + b = IOAction' . String' $ "There is no overload for " ++ show a ++ " and " ++ show b

(Number' a) - (Number' b) = Number' $ (Prelude.-) a b
(Number' a) * (Number' b) = Number' $ (Prelude.*) a b
(Number' a) / (Number' b) = Number' $ a `div` b

callFunc :: Scope -> String -> [RuntimeValue] -> (Scope, RuntimeValue)
callFunc s "+" as@((Number' _) : _) = (s, foldl (Main.+) (Number' 0) as)
callFunc s "+" (a@(List' _) : as) = (s, foldl (Main.+) a as)
callFunc s "-" (a : as) = (s, foldl (Main.-) a as)
callFunc s "*" as@((Number' _) : _) = (s, foldl (Main.*) (Number' 1) as)
callFunc s "/" (a@(Number' _) : as) = (s, foldl (Main./) a as)
callFunc s "=" [a, b] = (s, Number' . fromEnum $ a == b)
callFunc s ">" [Number' a, Number' b] = (s, Number' . fromEnum $ a > b)
callFunc s ">=" [Number' a, Number' b] = (s, Number' . fromEnum $ a >= b)
callFunc s "<" [Number' a, Number' b] = (s, Number' . fromEnum $ a < b)
callFunc s "<=" [Number' a, Number' b] = (s, Number' . fromEnum $ a <= b)
callFunc s "car" [(List' a)] = (s, head a)
callFunc s "cdr" [(List' a)] = (s, List' $ tail a)
callFunc s "list" l = (s, List' $ l)
callFunc s "cons" l = (s, case last l of
                            List' la -> List' $ foldr (:) la $ init l
                            _ -> error "Calling cons on a non array value")
callFunc s "empty?" [List' l] = (s, Number' . fromEnum $ null l)
callFunc s "or" (a : as) = (s, Number' . fromEnum $ foldl (\a b -> a || thruthy b) (thruthy a) as)
callFunc s "and" (a : as) = (s, Number' . fromEnum $ foldl (\a b -> a && thruthy b) (thruthy a) as)
callFunc s "append" l = callFunc s "+" l
callFunc s "print" [a] = (s, IOAction' a)
callFunc s name args =
  case Map.lookup name s of
    Just (Lambda' as b) -> exec (Map.union (Map.fromList $ zip as args) s) b
    _ -> (s, IOAction' . String' $ "Unknown function " ++ name ++ " with args " ++ show args)

thruthy :: RuntimeValue -> Bool
thruthy (Number' 0)  = False
thruthy _            = True

exec :: Scope -> Atom -> (Scope, RuntimeValue)
exec s (List [Identifier "lambda", List args, e]) =
    (s, Lambda' (map (\(Identifier i) -> i) args) e)
exec s (List [Identifier "setq", Identifier name, v]) = (Map.alter (\b ->
    case b of
      Just c -> b
      Nothing -> Just . snd $ exec s v) name s, Nil)
exec s (List [Identifier "defun", Identifier name, List args, e]) =
    exec s $ List [Identifier "setq", Identifier name,
      (List [Identifier "lambda", List args, e])]
exec s (List [Identifier "if", cond, t, e]) =
    if thruthy . snd $ exec s cond then exec s t else exec s e
exec s (Identifier "nil") = (s, Nil)
exec s (List ((Identifier a) : as)) = callFunc s a $ map (snd . exec s) as
exec s (Number n) = (s, Number' n)
exec s (StringLiteral l) = (s, String' l)
exec s (Quote (List q)) = (s, List' $ map (snd . exec s) q)
exec s (Identifier n) = (s, case Map.lookup n s of
                          Just e -> e
                          Nothing -> Nil)
exec s a = (s, IOAction' . String' $ "Called non-exhaustive pattern of exec with " ++ show a)

main :: IO ()
main = do
  args <- getArgs
  source <- (readFile $ args !! 0)
  case parse file source of
    Just (r, as) -> do
      _ <- foldM (\s a -> do
        let (s', v) = exec s a
        case v of
          IOAction' t -> putStrLn . show $ t
          List' ts@(IOAction' _ : _) -> mapM_ (putStrLn . show) ts
          _ -> return ()
        return s'
        ) (Map.empty) as
      return ()
    Nothing -> empty
  where
    foldM :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a
    foldM _ z [] = return z
    foldM f z (x:xs) = do
      z' <- f z x
      z' `seq` foldM f z' xs
