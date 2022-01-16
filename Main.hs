module Main where

import qualified Data.Map.Strict as Map
import Parser
import System.Environment

type Scope = Map.Map String RuntimeValue

data RuntimeValue
  = Number' Int
  | Char' Char
  | List' [RuntimeValue]
  | Lambda' Scope [String] Atom
  | Nil'
  | Intrinsic'
      Int
      String
      (Scope -> [RuntimeValue] -> IO (Scope, RuntimeValue))

instance Eq RuntimeValue where
  (Number' a) == (Number' b) = a == b
  (List' a) == (List' b) = a == b
  (Lambda' _ _ _) == (Lambda' _ _ _) = False
  (Intrinsic' _ _ _) == (Intrinsic' _ _ _) = False
  (Char' a) == (Char' b) = a == b
  Nil' == Nil' = True
  _ == _ = False

instance Show Atom where
  show (List (x:xs)) =
    "(" ++
    (foldl (\a e -> a ++ " " ++ show e) (show x) xs) ++ ")"
  show (Number n) = show n
  show (Identifier i) = i
  show (StringLiteral s) = "\"" ++ s ++ "\""
  show (Quote a) = "'" ++ show a

instance Show RuntimeValue where
  show (Number' a) = show a
  show (List' s@(Char' _:_)) = map (\(Char' c) -> c) s
  show (List' []) = "()"
  show (List' a) =
    "(" ++
    (show . head $ a) ++
    (foldl (\a e -> a ++ " " ++ show e) "" $ tail a) ++ ")"
  show Nil' = "Nil"
  show (Lambda' _ [] b) = "(lambda () " ++ show b ++ ")"
  show (Lambda' _ args b) =
    "(lambda (" ++
    foldl (\c a -> c ++ " " ++ a) (head args) (tail args) ++
    ") " ++ show b ++ ")"
  show (Intrinsic' _ n _) = "(intrinsic " ++ n ++ ")"
  show (Char' c) = [c]

standardScope :: Scope
standardScope =
  Map.fromList
    [ ( "+"
      , Intrinsic' 2 "+" $ \s (a:as) ->
          return
            ( s
            , foldl
                (\(Number' a) (Number' b) -> Number' $ a + b)
                a
                as))
    , ( "-"
      , Intrinsic' 2 "-" $ \s (a:as) ->
          return
            ( s
            , foldl
                (\(Number' a) (Number' b) -> Number' $ a - b)
                a
                as))
    , ( "*"
      , Intrinsic' 2 "*" $ \s (a:as) ->
          return
            ( s
            , foldl
                (\(Number' a) (Number' b) -> Number' $ a * b)
                a
                as))
    , ( "/"
      , Intrinsic' 2 "/" $ \s (a:as) ->
          return
            ( s
            , foldl
                (\(Number' a) (Number' b) ->
                   Number' $ a `div` b)
                a
                as))
    , ( "="
      , Intrinsic' 2 "=" $ \s [a, b] ->
          return (s, Number' . fromEnum $ a == b))
    , ( "<"
      , Intrinsic' 2 "<" $ \s [Number' a, Number' b] ->
          return (s, Number' . fromEnum $ a < b))
    , ( "<="
      , Intrinsic' 2 "<=" $ \s [Number' a, Number' b] ->
          return (s, Number' . fromEnum $ a <= b))
    , ( ">"
      , Intrinsic' 2 ">" $ \s [Number' a, Number' b] ->
          return (s, Number' . fromEnum $ a > b))
    , ( ">="
      , Intrinsic' 2 ">=" $ \s [Number' a, Number' b] ->
          return (s, Number' . fromEnum $ a >= b))
    , ( "car"
      , Intrinsic' 1 "car" $ \s [List' l] ->
          return (s, head l))
    , ( "cdr"
      , Intrinsic' 1 "cdr" $ \s [List' l] ->
          case l of
            (_:_) -> return (s, List' $ tail l)
            _ -> print l >>= \_ -> return (s, List' l))
    , ( "list"
      , Intrinsic' 1 "list" $ \s as -> return (s, List' as))
    , ( "cons"
      , Intrinsic' 2 "cons" $ \s as ->
          return
            ( s
            , case last as of
                List' la -> List' $ foldr (:) la $ init as
                _ -> Nil'))
    , ( "null"
      , Intrinsic' 1 "null" $ \s l ->
          return
            ( s
            , case l of
                [List' al] -> Number' . fromEnum $ null al
                _ -> Nil'))
    , ( "or"
      , Intrinsic' 2 "or" $ \s (a:as) ->
          return
            ( s
            , Number' . fromEnum $
              foldl (\a b -> a || thruthy b) (thruthy a) as))
    , ( "and"
      , Intrinsic' 2 "and" $ \s (a:as) ->
          return
            ( s
            , Number' . fromEnum $
              foldl (\a b -> a && thruthy b) (thruthy a) as))
    , ( "print"
      , Intrinsic' 1 "print" $ \s [t] -> do
          putStrLn $ show t
          return (s, Nil'))
    , ( "char"
      , Intrinsic' 1 "char" $ \s [t] ->
          case t of
            List' [Char' a] -> return (s, Char' a)
            _ -> return (s, Nil'))
    ]

thruthy :: RuntimeValue -> Bool
thruthy (Number' 0) = False
thruthy Nil' = False
thruthy _ = True

exec :: Scope -> Atom -> IO (Scope, RuntimeValue)
exec s (List [Identifier "lambda", List args, e]) =
  return (s, Lambda' s (map (\(Identifier i) -> i) args) e)
exec s (List [Identifier "setq", Identifier name, v]) = do
  (_, v) <- exec s v
  return
    ( Map.alter
        (\b ->
           case b of
             Just c -> b
             Nothing -> Just v)
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
exec s (List [Identifier "if", cond, t, e]) = do
  (_, v) <- exec s cond
  if thruthy v
    then exec s t
    else exec s e
exec s (Identifier "nil") = return (s, Nil')
exec s (List (f:atoms)) = do
  vs <- sequence $ map (\a -> exec s a) atoms
  let s' = Map.unions $ map fst vs
  let args = map snd vs
  (_, v) <- exec s f
  case v of
    (Lambda' ls ps b) ->
      if length args < length ps
        then let cps =
                   [ Identifier $ "$" ++ show i
                   | i <- [1 .. length ps - length args]
                   ]
              in exec (Map.union ls s') $
                 List
                   [ Identifier "lambda"
                   , List cps
                   , List $ [f] ++ atoms ++ cps
                   ]
        else exec
               (Map.unions
                  [Map.fromList $ zip ps args, ls, s'])
               b
    i@(Intrinsic' np nm f) ->
      let curriedParams = max 0 $ np - length atoms
       in if curriedParams > 0
            then let ps =
                       [ "$" ++ show i
                       | i <- [1 .. curriedParams]
                       ]
                  in return
                       ( s
                       , Lambda' Map.empty ps $
                         List $
                         [Identifier nm] ++
                         atoms ++ map Identifier ps)
            else f s' args
    t -> do
      putStrLn $ "Calling non-function " ++ show t
      return (s, Nil')
exec s (Number n) = return (s, Number' n)
exec s (StringLiteral l) = return (s, List' $ map (Char') l)
exec s (Quote (List q)) = do
  l <- sequence $ map (\e -> exec s e >>= return . snd) q
  return (s, List' l)
exec s (Identifier n) =
  return
    ( s
    , case Map.lookup n s of
        Just e -> e
        Nothing -> Nil')
exec s a = return (s, Nil')

main :: IO ()
main = do
  args <- getArgs
  source <- (readFile $ args !! 0)
  case parse file source of
    Just (r, as) -> do
      foldl
        (\s a -> do
           s' <- s
           (s'', v) <- exec s' a
           return s'')
        (pure standardScope)
        as
      return ()
    Nothing -> return ()
