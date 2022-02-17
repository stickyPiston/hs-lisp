module Value where

import Control.Monad.Trans.Except (ExceptT)
import Data.Map.Strict (Map)

import Parser

type Result = ExceptT String IO (Context, Value)

data Value
  = Number Int
  | Char Char
  | Lambda Context String Atom
  | Intrinsic ([Value] -> ExceptT String IO Value)
  | Bool Bool
  | Symbol String
  | ValueList [Value]
  | Nil

type Context = Map String Value

class Typeof a where
  typeof :: a -> String

instance Show Atom where
  show (List (x:xs)) =
    "(" ++
    (foldl (\a e -> a ++ " " ++ show e) (show x) xs) ++ ")"
  show (List []) = "()"
  show (Parser.Number n) = show n
  show (Identifier i) = i
  show (StringLiteral s) = "\"" ++ s ++ "\""
  show (Quote a) = "'" ++ show a
  show (Parser.Bool True) = "#t"
  show (Parser.Bool False) = "#f"
  show Wildcard = "_"

isChar :: Value -> Bool
isChar (Char _) = True
isChar _ = False

instance Show Value where
  show (Value.Number a) = show a
  show (ValueList a)
    | all isChar a = map (\(Char c) -> c) a
    | otherwise = "(" ++ (unwords $ map show a) ++ ")"
  show Nil = "nil"
  show (Lambda _ arg b) =
    "(lambda (" ++ arg ++ ") " ++ show b ++ ")"
  show (Intrinsic _) = "(intrinsic)"
  show (Symbol s) = '\'' : s
  show (Value.Bool True) = "#t"
  show (Value.Bool False) = "#f"
  show (Char c) = [c]

instance Eq Value where
  (Value.Number a) == (Value.Number b) = a == b
  (ValueList a) == (ValueList b) = a == b
  (Lambda _ _ _) == (Lambda _ _ _) = False
  (Intrinsic _) == (Intrinsic _) = False
  Nil == Nil = True
  (Symbol a) == (Symbol b) = a == b
  (Value.Bool a) == (Value.Bool b) = a == b
  _ == _ = False

instance Typeof Atom where
  typeof (List _) = "list"
  typeof (Parser.Number _) = "number"
  typeof (Identifier _) = "identifier"
  typeof (StringLiteral _) = "string literal"
  typeof (Quote _) = "quoted value"
  typeof (Parser.Bool _) = "boolean"
  typeof Wildcard = "wildcard"

instance Typeof Value where
  typeof (Value.Number _) = "number"
  typeof (ValueList _) = "list"
  typeof (Lambda _ _ _) = "lambda"
  typeof Nil = "nil"
  typeof (Symbol _) = "symbol"
  typeof (Intrinsic _ ) = "intrinsic"
  typeof (Value.Bool _) = "boolean"
  typeof (Char _) = "char"

thruthy :: Value -> Bool
thruthy (Value.Bool False) = False
thruthy _ = True
