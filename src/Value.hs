module Value where

import Control.Monad.Trans.Except (ExceptT)
import Data.Map.Strict (Map)
import System.IO (Handle)

import Parser

type Result = ExceptT String IO (Context, Value)

data Value
  = Number Int
  | Char Char
  | Lambda Context String Expression
  | Intrinsic ([Value] -> ExceptT String IO Value)
  | Bool Bool
  | Symbol String
  | List [Value]
  | Handle Handle

type Context = Map String Value

class Typeof a where
  typeof :: a -> String

isChar :: Value -> Bool
isChar (Value.Char _) = True
isChar _ = False

instance Show Value where
  show (Value.Number a) = show a
  show (Value.List a)
    | all isChar a = map (\(Value.Char c) -> c) a
    | otherwise = "(" ++ (unwords $ map show a) ++ ")"
  show (Lambda _ arg b) =
    "(λ (" ++ arg ++ ") " ++ show b ++ ")"
  show (Intrinsic _) = "(intrinsic)"
  show (Symbol s) = '\'' : s
  show (Value.Bool True) = "#t"
  show (Value.Bool False) = "#f"
  show (Value.Char c) = [c]
  show (Handle h) = "(handle)"

instance Eq Value where
  (Value.Number a) == (Value.Number b) = a == b
  (Value.List a) == (Value.List b) = a == b
  (Lambda _ _ _) == (Lambda _ _ _) = False
  (Intrinsic _) == (Intrinsic _) = False
  (Symbol a) == (Symbol b) = a == b
  (Value.Bool a) == (Value.Bool b) = a == b
  _ == _ = False

instance Typeof Expression where
  typeof (Parser.List _) = "list"
  typeof (Parser.Number _) = "number"
  typeof (Identifier _) = "identifier"
  typeof (StringLiteral _) = "string literal"
  typeof (Quote _) = "quoted value"
  typeof (Parser.Bool _) = "boolean"
  typeof Wildcard = "wildcard"
  typeof (Abst _ _) = "Abstraction"
  typeof (Appl _ _) = "Application"

instance Typeof Statement where
  typeof (Define _ _ _) = "Definition"
  typeof (Import _ _ _) = "Import"

instance Typeof Value where
  typeof (Value.Number _) = "number"
  typeof (Value.List _) = "list"
  typeof (Lambda _ _ _) = "lambda"
  typeof (Symbol _) = "symbol"
  typeof (Intrinsic _ ) = "intrinsic"
  typeof (Value.Bool _) = "boolean"
  typeof (Value.Char _) = "char"
  typeof (Handle _) = "handle"

thruthy :: Value -> Bool
thruthy (Value.Bool False) = False
thruthy _ = True
