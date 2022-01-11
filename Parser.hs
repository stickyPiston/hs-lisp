module Parser where

import Control.Applicative

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

