module Value where

import Control.Monad.Trans.Except (ExceptT)
import Data.Map.Strict (Map)
import System.IO (Handle)

import Parser

type Result = ExceptT String IO (Context, Value)

data Value
  = Number Int
  | Char Char
  | Lambda Context String Atom
  | Intrinsic ([Value] -> ExceptT String IO Value)
  | Bool Bool
  | Symbol String
  | List [Value]
  | Handle Handle

type Context = Map String Value

class Typeof a where
  typeof :: a -> String

instance Show Atom where
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
  show (Define False n t) = "(define " ++ n ++ " " ++ show t ++ ")"  
  show (Define True n t) = "(define-rec " ++ n ++ " " ++ show t ++ ")"  

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

instance Typeof Atom where
  typeof (Parser.List _) = "list"
  typeof (Parser.Number _) = "number"
  typeof (Identifier _) = "identifier"
  typeof (StringLiteral _) = "string literal"
  typeof (Quote _) = "quoted value"
  typeof (Parser.Bool _) = "boolean"
  typeof Wildcard = "wildcard"
  typeof (Abst _ _) = "Abstraction"
  typeof (Appl _ _) = "Application"
  typeof (Define _ _ _) = "Definition"

instance Typeof Value where
  typeof (Value.Number _) = "number"
  typeof (Value.List _) = "list"
  typeof (Lambda _ _ _) = "lambda"
  typeof (Symbol _) = "symbol"
  typeof (Intrinsic _ ) = "intrinsic"
  typeof (Value.Bool _) = "boolean"
  typeof (Value.Char _) = "char"

thruthy :: Value -> Bool
thruthy (Value.Bool False) = False
thruthy _ = True
