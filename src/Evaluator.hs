{-# LANGUAGE LambdaCase #-}

module Evaluator where

import Data.Char (chr, isSpace, ord)
import Data.Either (isLeft, lefts, rights, fromRight)
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing)
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

instance Show ParsedAtom where
  show a = show $ atm a

instance Typeof Atom where
  typeof (List _) = "list"
  typeof (Number _) = "number"
  typeof (Identifier _) = "identifier"
  typeof (StringLiteral _) = "string literal"
  typeof (Quote _) = "quoted value"
  typeof (Bool _) = "boolean"

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

standardScope :: Scope
standardScope =
  Map.fromList
    [ generateOperator "+" (+)
    , generateOperator "-" (-)
    , generateOperator "*" (*)
    , generateOperator "/" div
    , generateOperator "<" $ (fromEnum .) . (<)
    , generateOperator "<=" $ (fromEnum .) . (<=)
    , generateOperator ">" $ (fromEnum .) . (>)
    , generateOperator ">=" $ (fromEnum .) . (>=)
    , ( "="
      , Intrinsic' 2 "=" $ \s as ->
          return $
          case as of
            [a, b] -> Right (s, Number' . fromEnum $ a == b)
            _ ->
              Left
                [ "Incorrect number of arguments in call to ="
                ])
    , ( "car"
      , Intrinsic' 1 "car" $
        curry
          (\case
             (s, [List' l]) -> return . Right $ (s, head l)
             (s, [v]) ->
               return . Left $
               [ "Cannot convert " ++
                 typeof v ++ " to List in call to car"
               ]
             (_, _) ->
               return . Left $
               [ "Incorrect number of arguments in call to car"
               ]))
    , ( "cdr"
      , Intrinsic' 1 "cdr" $
        curry
          (\case
             (s, [List' l]) ->
               return . Right $ (s, List' $ tail l)
             (s, [v]) ->
               return . Left $
               [ "Cannot convert " ++
                 typeof v ++ " to List in call to cdr"
               ]
             (_, _) ->
               return . Left $
               [ "Incorrect number of arguments in call to cdr"
               ]))
    , ( "list"
      , Intrinsic' 1 "list" $ \s as ->
          return . Right $ (s, List' as))
    , ( "print-context"
      , Intrinsic' 0 "print-context" $ \s _ -> do
          print s
          return . Right $ (s, Number' 0))
    , ( "cons"
      , Intrinsic' 2 "cons" $ \s as ->
          return $
          case last as of
            List' la ->
              Right $ (s, List' $ foldr (:) la $ init as)
            _ ->
              Left
                [ "Last argument to cons must a be list, but received " ++
                  typeof (last as)
                ])
    , ( "null"
      , Intrinsic' 1 "null" $
        curry
          (\case
             (s, [List' l]) ->
               return . Right $
               (s, Number' . fromEnum $ null l)
             (_, [v]) ->
               return . Left $
               [ "Cannot convert " ++
                 typeof v ++ " to List in call to null"
               ]
             (_, v) ->
               return . Left $
               [ "Incorrect number of arguments in call to cdr, expected 1, got " ++
                 show (length v)
               ]))
    , ( "or"
      , Intrinsic' 2 "or" $ \s (a:as) ->
          return . Right $
          ( s
          , Number' . fromEnum $
            foldl (\a b -> a || thruthy b) (thruthy a) as))
    , ( "and"
      , Intrinsic' 2 "and" $ \s (a:as) ->
          return . Right $
          ( s
          , Number' . fromEnum $
            foldl (\a b -> a && thruthy b) (thruthy a) as))
    , ( "print"
      , Intrinsic' 1 "print" $ \s [t] -> do
          putStrLn $ show t
          return . Right $ (s, t))
    , ( "char"
      , Intrinsic' 1 "char" $
        curry
          (\case
             (s, [List' [Char' a]]) ->
               return . Right $ (s, Char' a)
             (s, [v]) ->
               return . Left $
               [ "Cannot convert " ++
                 typeof v ++ " to List in call to char"
               ]
             (_, v) ->
               return . Left $
               [ "Incorrect number of arguments in call to char, expected 1, got " ++
                 show (length v)
               ]))
    , ( "to-ascii"
      , Intrinsic' 1 "to-ascii" $
        curry
          (\case
             (s, [Number' n]) ->
               return . Right $ (s, Char' $ chr n)
             (s, [v]) ->
               return . Left $
               [ "Cannot convert " ++
                 typeof v ++
                 " to Number in call to to-ascii"
               ]
             (_, v) ->
               return . Left $
               [ "Incorrect number of arguments in call to to-ascii, expected 1, got " ++
                 show (length v)
               ]))
    , ( "from-ascii"
      , Intrinsic' 1 "to-ascii" $
        curry
          (\case
             (s, [Char' n]) ->
               return . Right $ (s, Number' $ ord n)
             (s, [v]) ->
               return . Left $
               [ "Cannot convert " ++
                 typeof v ++
                 " to Number in call to to-ascii"
               ]
             (_, v) ->
               return . Left $
               [ "Incorrect number of arguments in call to to-ascii, expected 1, got " ++
                 show (length v)
               ]))
    , ( "throw"
      , Intrinsic' 1 "throw" $
        curry
          (\case
             (s, [List' cs]) ->
               let ecs =
                     map
                       (\case
                          Char' c -> Right c
                          v ->
                            Left $
                            "Cannot convert " ++
                            typeof v ++
                            " to Char in call to throw")
                       cs
                in return . Left $
                   if any isLeft ecs
                     then lefts ecs
                     else [rights ecs]
             (s, [v]) ->
               return . Left $
               [ "Cannot convert " ++
                 typeof v ++ " to List in call to throw"
               ]
             (_, v) ->
               return . Left $
               [ "Incorrect number of arguments in call to throw, expected 1, got " ++
                 show (length v)
               ]))
    ]
  where
    generateOperator ::
         String
      -> (Int -> Int -> Int)
      -> (String, RuntimeValue)
    generateOperator name op =
      ( name
      , Intrinsic' 2 name $ \s as ->
          let ns =
                map
                  (\case
                     (Number' n) -> Right $ n
                     a ->
                       Left $
                       "Cannot convert " ++
                       typeof a ++
                       " to Number in call to " ++ name)
                  as
           in return $
              if any isLeft ns
                then Left $ lefts ns
                else let rns = rights ns
                      in Right
                           ( s
                           , Number' $
                             foldl op (head rns) (tail rns)))

thruthy :: RuntimeValue -> Bool
thruthy (Number' 0) = False
thruthy Nil' = False
thruthy _ = True

exec ::
     Scope
  -> Atom
  -> IO (Either [String] (Scope, RuntimeValue))
exec s (List [Identifier "λ", args, e]) =
  exec s (List [Identifier "lambda", args, e])
exec s (List [Identifier "lambda", args, e]) =
  case args of
    List l -> 
      let params = map getName l
       in return $
          if any isLeft params
            then Left . concat $ lefts params
            else Right (s, Lambda' s (rights params) e)
    Identifier i -> 
      return . Right $ (s, NaryLambda' s i e)
  where
    getName (Identifier i) = Right i
    getName a =
      Left
        [ "Expected an identifier in parameter list, instead got " ++
          show a
        ]
exec s (List [Identifier "setq", Identifier name, v]) = do
  ev <- exec s v
  case ev of
    Right (_, v) ->
      return . Right $
      ( Map.alter
          (\b ->
             case b of
               Just c -> b
               Nothing -> Just v)
          name
          s
      , Nil')
    e -> return e
exec s (List [Identifier "defun", Identifier name, args, e]) =
  exec s $
  List
    [ Identifier "setq"
    , Identifier name
    , (List [Identifier "lambda", args, e])
    ]
exec s (List [Identifier "if", cond, t, e]) = do
  ev <- exec s cond
  case ev of
    Right (_, v) ->
      if thruthy v
        then exec s t
        else exec s e
    e -> return e
exec s (List [Identifier "let", List bindings, prog]) = do
  let exbs =
        map
          (\case
             List [Identifier i, a] -> (i, exec s a)
             _ ->
               ( ""
               , return . Left $ ["Malformed let statement"]))
          bindings
  ebs <- sequence $ map snd exbs
  let is = map fst exbs
  if any isLeft ebs
    then return . Left . concat . lefts $ ebs
    else let s' =
               foldl (\m (n, v) -> Map.insert n v m) s .
               zip is . map snd . rights $
               ebs
          in exec s' prog
exec s (Identifier "nil") = return . Right $ (s, Nil')
exec s (List (f:atoms)) = do
  vs <- sequence $ map (exec s) atoms
  if any isLeft vs
    then return . Left . concat $ lefts vs
    else do
      let s' = Map.unions . map fst $ rights vs
      let args = map snd $ rights vs
      r <- exec s f
      case r of
        Right (_, v) ->
          case v of
            (Lambda' ls ps b) ->
              if length args < length ps
                then let cps =
                           [ Identifier $ "$" ++ show i
                           | i <-
                               [1 .. length ps - length args]
                           ]
                      in exec (Map.union ls s') $
                         List
                           [ Identifier "lambda"
                           , List cps
                           , List $ [f] ++ atoms ++ cps
                           ]
                else exec
                       (Map.unions
                          [ Map.fromList $ zip ps args
                          , ls
                          , s'
                          ])
                       b >>= \case
                       Left es ->
                         return . Left $
                         map
                           (++ "\nIn evaluation of " ++
                               show (List $ f : atoms))
                           es
                       v -> return v
            (NaryLambda' ls i b) ->
              -- FIXME: Figure out currying
              let ns = Map.unions [Map.singleton i $ List' args, ls, s]
               in exec ns b
            (Intrinsic' np nm f) ->
              let curriedParams = max 0 $ np - length atoms
               in if curriedParams > 0
                    then let ps =
                               [ "$" ++ show i
                               | i <- [1 .. curriedParams]
                               ]
                          in return . Right $
                             ( s
                             , Lambda' Map.empty ps $
                               List $
                               [Identifier nm] ++
                               atoms ++ map Identifier ps)
                    else f s' args >>= \case
                           Left es ->
                             return . Left $
                             map
                               (++ "\nIn evalutation of " ++
                                   nm)
                               es
                           v -> return v
            t -> do
              return . Left $
                ["Calling non-function " ++ show t]
        e -> return e
exec s (Number n) = return . Right $ (s, Number' n)
exec s (StringLiteral l) =
  return . Right $ (s, List' $ map (Char') l)
exec s (Quote a) =
  case a of
    List q -> do
      l <- sequence $ map (exec s) q
      if any isLeft l
        then return . Left . concat $ lefts l
        else return . Right $
             (s, List' . map snd $ rights l)
    Identifier i -> return . Right $ (s, Symbol' i)
    Number n -> return . Right $ (s, Number' n)
    _ ->
      return . Left $ ["Quoting this value is not allowed"]
exec s (Identifier n) =
  return . Right $
  ( s
  , case Map.lookup n s of
      Just e -> e
      Nothing -> Nil')
exec s (Bool b) = return . Right $ (s, Bool' b)
exec s a =
  return . Left $
  ["Parsing error: unknown sequence of tokens " ++ show a]
