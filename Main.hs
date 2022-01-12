module Main where

import Parser

import Control.DeepSeq
import qualified Data.Map.Strict as Map
import System.Environment
import System.IO
import System.IO.Unsafe

type Scope = Map.Map String RuntimeValue

data RuntimeValue
  = Number' Int
  | Char' Char
  | List' [RuntimeValue]
  | Lambda' [String] Atom
  | Nil'
  | Intrinsic'
      (Scope -> [RuntimeValue] -> (Scope, RuntimeValue))

instance NFData RuntimeValue where
  rnf x = seq x ()

instance Eq RuntimeValue where
  (Number' a) == (Number' b) = a == b
  (List' a) == (List' b) = a == b
  (Lambda' _ _) == (Lambda' _ _) = False
  (Intrinsic' _) == (Intrinsic' _) = False
  (Char' a) == (Char' b) = a == b
  Nil' == Nil' = True
  _ == _ = False

instance Show RuntimeValue where
  show (Number' a) = show a
  show (List' s@(Char' _:_)) = map (\(Char' c) -> c) s
  show (List' []) = "()"
  show (List' a) =
    "(" ++
    (show . head $ a) ++
    (foldl (\a e -> a ++ " " ++ show e) "" $ tail a) ++ ")"
  show Nil' = "Nil"
  show (Lambda' _ _) = "Lambda"
  show (Intrinsic' _) = "Intrinsic"
  show (Char' c) = [c]

standardScope :: Scope
standardScope =
  Map.fromList
    [ ( "+"
      , Intrinsic' $ \s (a:as) ->
          ( s
          , foldl
              (\(Number' a) (Number' b) -> Number' $ a + b)
              a
              as))
    , ( "-"
      , Intrinsic' $ \s (a:as) ->
          ( s
          , foldl
              (\(Number' a) (Number' b) -> Number' $ a - b)
              a
              as))
    , ( "*"
      , Intrinsic' $ \s (a:as) ->
          ( s
          , foldl
              (\(Number' a) (Number' b) -> Number' $ a * b)
              a
              as))
    , ( "/"
      , Intrinsic' $ \s (a:as) ->
          ( s
          , foldl
              (\(Number' a) (Number' b) ->
                 Number' $ a `div` b)
              a
              as))
    , ( "="
      , Intrinsic' $ \s [a, b] ->
          (s, Number' . fromEnum $ a == b))
    , ( "<"
      , Intrinsic' $ \s [Number' a, Number' b] ->
          (s, Number' . fromEnum $ a < b))
    , ( "<="
      , Intrinsic' $ \s [Number' a, Number' b] ->
          (s, Number' . fromEnum $ a <= b))
    , ( ">"
      , Intrinsic' $ \s [Number' a, Number' b] ->
          (s, Number' . fromEnum $ a > b))
    , ( ">="
      , Intrinsic' $ \s [Number' a, Number' b] ->
          (s, Number' . fromEnum $ a >= b))
    , ("car", Intrinsic' $ \s [List' l] -> (s, head l))
    , ( "cdr"
      , Intrinsic' $ \s [List' l] -> (s, List' $ tail l))
    , ("list", Intrinsic' $ \s as -> (s, List' as))
    , ( "cons"
      , Intrinsic' $ \s as ->
          ( s
          , case last as of
              List' la -> List' $ foldr (:) la $ init as
              _ -> Nil'))
    , ( "null"
      , Intrinsic' $ \s l ->
          ( s
          , case l of
              [List' al] -> Number' . fromEnum $ null al
              _ -> Nil'))
    , ( "or"
      , Intrinsic' $ \s (a:as) ->
          ( s
          , Number' . fromEnum $
            foldl (\a b -> a || thruthy b) (thruthy a) as))
    , ( "and"
      , Intrinsic' $ \s (a:as) ->
          ( s
          , Number' . fromEnum $
            foldl (\a b -> a && thruthy b) (thruthy a) as))
    , ( "print" -- Make this not use unsafePerformIO
      , Intrinsic' $ \s [t] ->
          unsafePerformIO $ do
            putStrLn $ show t
            hFlush stdout
            return (s, t))
    , ( "char"
      , Intrinsic' $ \s [t] ->
          case t of
            List' [Char' a] -> (s, Char' a)
            _ -> (s, Nil'))
    ]

thruthy :: RuntimeValue -> Bool
thruthy (Number' 0) = False
thruthy Nil' = False
thruthy _ = True

exec :: Scope -> Atom -> (Scope, RuntimeValue)
exec s (List [Identifier "lambda", List args, e]) =
  (s, Lambda' (map (\(Identifier i) -> i) args) e)
exec s (List [Identifier "setq", Identifier name, v]) =
  ( Map.alter
      (\b ->
         case b of
           Just c -> b
           Nothing -> Just . snd $ exec s v)
      name
      s
  , Nil')
exec s (List [Identifier "defun", Identifier name, List args, e]) =
  exec s $
  List
    [ Identifier "setq"
    , Identifier name
    , (List [Identifier "lambda", List args, e])
    ]
exec s (List [Identifier "if", cond, t, e]) =
  if thruthy . snd $ exec s cond
    then exec s t
    else exec s e
exec s (Identifier "nil") = (s, Nil')
exec s (List (f:atoms)) =
  let args = map (snd . exec s) atoms
   in case snd $ exec s f of
        (Lambda' ps b) ->
          exec (Map.union (Map.fromList $ zip ps args) s) b
        (Intrinsic' f) -> f s args
        _ -> (s, Nil')
exec s (Number n) = (s, Number' n)
exec s (StringLiteral l) = (s, List' $ map (Char') l)
exec s (Quote (List q)) = (s, List' $ map (snd . exec s) q)
exec s (Identifier n) =
  ( s
  , case Map.lookup n s of
      Just e -> e
      Nothing -> Nil')
exec s a = (s, Nil')

main :: IO ()
main = do
  args <- getArgs
  source <- (readFile $ args !! 0)
  let a =
        parse file source >>= \(_, as) ->
          return $
          foldl (\s a -> fst $ exec s a) standardScope as
  a `deepseq` return ()
