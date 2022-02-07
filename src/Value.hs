module Value where

import qualified Data.Map.Strict as Map
import Parser

type Scope = Map.Map String RuntimeValue

data RuntimeValue
  = Number' Int
  | Char' Char
  | Bool' Bool
  | List' [RuntimeValue]
  | Lambda' Scope [String] Atom
  | NaryLambda' Scope String Atom
  | Nil'
  | Symbol' String
  | Intrinsic'
      Int
      String
      (Scope -> [RuntimeValue] -> IO (Either [String] ( Scope
                                                      , RuntimeValue)))

class Typeof a where
  typeof :: a -> String

instance Show Atom where
  show (List (x:xs)) =
    "(" ++
    (foldl (\a e -> a ++ " " ++ show e) (show x) xs) ++ ")"
  show (List []) = "()"
  show (Number n) = show n
  show (Identifier i) = i
  show (StringLiteral s) = "\"" ++ s ++ "\""
  show (Quote a) = "'" ++ show a
  show (Bool True) = "#t"
  show (Bool False) = "#f"
  show Wildcard = "_"

instance Show ParsedAtom where
  show a = show $ atm a

instance Typeof Atom where
  typeof (List _) = "list"
  typeof (Number _) = "number"
  typeof (Identifier _) = "identifier"
  typeof (StringLiteral _) = "string literal"
  typeof (Quote _) = "quoted value"
  typeof (Bool _) = "boolean"
  typeof Wildcard = "wildcard"

instance Eq RuntimeValue where
  (Number' a) == (Number' b) = a == b
  (List' a) == (List' b) = a == b
  (Lambda' _ _ _) == (Lambda' _ _ _) = False
  (Intrinsic' _ _ _) == (Intrinsic' _ _ _) = False
  (Char' a) == (Char' b) = a == b
  Nil' == Nil' = True
  (Symbol' a) == (Symbol' b) = a == b
  (Bool' a) == (Bool' b) = a == b
  _ == _ = False

instance Show RuntimeValue where
  show (Number' a) = show a
  show (List' s@(Char' _:_)) = "\"" ++ map (\(Char' c) -> c) s ++ "\""
  show (List' a) = "(" ++ (unwords $ map show a) ++ ")"
  show Nil' = "Nil"
  show (Lambda' _ [] b) = "(lambda () " ++ show b ++ ")"
  show (Lambda' _ args b) =
    "(lambda (" ++
    unwords args ++    ") " ++ show b ++ ")"
  show (Intrinsic' _ n _) = "(intrinsic " ++ n ++ ")"
  show (Char' c) = [c]
  show (Symbol' s) = '\'' : s
  show (Bool' True) = "#t"
  show (Bool' False) = "#f"

instance Typeof RuntimeValue where
  typeof (Number' _) = "number"
  typeof (List' _) = "list"
  typeof (Lambda' _ _ _) = "lambda"
  typeof Nil' = "nil"
  typeof (Char' _) = "char"
  typeof (Symbol' _) = "symbol"
  typeof (Intrinsic' _ _ _) = "intrinsic"
  typeof (Bool' _) = "boolean"

thruthy :: RuntimeValue -> Bool
thruthy (Number' 0) = False
thruthy Nil' = False
thruthy _ = True

