{-# LANGUAGE LambdaCase #-}

module Evaluator where

import Data.Either (fromRight, isLeft, lefts, rights)
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing)
import Intrinsics
import Parser
import Value

exec ::
     Scope -> Atom -> IO (Either [String] (Scope, RuntimeValue))
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
    Identifier i -> return . Right $ (s, NaryLambda' s i e)
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
               ("", return . Left $ ["Malformed let statement"]))
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
              let wildcards = filter isWildcard atoms
               in if length wildcards > 0
                    then let body =
                               foldl
                                 (\(n, r) a ->
                                    case a of
                                      Wildcard ->
                                        ( n + 1
                                        , (Identifier $
                                           "$" ++ show n) :
                                          r)
                                      _ -> (n, a : r))
                                 (0, [])
                                 atoms
                             params =
                               [ Identifier $ "$" ++ show n
                               | n <- [0 .. length wildcards]
                               ]
                          in exec (Map.union ls s') $
                             List
                               [ Identifier "lambda"
                               , List params
                               , List $ snd body
                               ]
                    else if length args < length ps
                           then let cps =
                                      [ Identifier $
                                      "$" ++ show i
                                      | i <-
                                          [1 .. length ps -
                                                length args]
                                      ]
                                 in exec (Map.union ls s') $
                                    List
                                      [ Identifier "lambda"
                                      , List cps
                                      , List $
                                        [f] ++ atoms ++ cps
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
            (NaryLambda' ls i b)
              -- FIXME: Figure out currying
             ->
              let ns =
                    Map.unions
                      [Map.singleton i $ List' args, ls, s]
               in exec ns b
               -- FIXME: Refactor the wildcards system
            (Intrinsic' np nm f) ->
              let wildcards = filter isWildcard atoms
               in if length wildcards > 0
                    then let body =
                               foldl
                                 (\(n, r) a ->
                                    case a of
                                      Wildcard ->
                                        ( n + 1
                                        , (Identifier $
                                           "$" ++ show n) :
                                          r)
                                      _ -> (n, a : r))
                                 (1, [])
                                 atoms
                             params =
                               [ Identifier $ "$" ++ show n
                               | n <- [1 .. length wildcards]
                               ]
                          in exec s' $ List
                               [ Identifier "lambda"
                               , List params
                               , List $ (Identifier nm) : (reverse $ snd body)
                               ]
                    else
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
                                       (++ "\nIn evalutation of " ++ nm)
                                       es
                                   v -> return v
            t -> do
              return . Left $ ["Calling non-function " ++ show t]
        e -> return e
  where
    isWildcard Wildcard = True
    isWildcard _ = False
exec s (Number n) = return . Right $ (s, Number' n)
exec s (StringLiteral l) =
  return . Right $ (s, List' $ map (Char') l)
exec s (Quote a) =
  case a of
    List q -> do
      l <- sequence $ map (exec s) q
      if any isLeft l
        then return . Left . concat $ lefts l
        else return . Right $ (s, List' . map snd $ rights l)
    Identifier i -> return . Right $ (s, Symbol' i)
    Number n -> return . Right $ (s, Number' n)
    _ -> return . Left $ ["Quoting this value is not allowed"]
exec s (Identifier n) =
  return . Right $
  ( s
  , case Map.lookup n s of
      Just e -> e
      Nothing -> Nil')
exec s (Bool b) = return . Right $ (s, Bool' b)
exec s Wildcard = return . Right $ (s, Nil')
exec s a =
  return . Left $
  ["Parsing error: unknown sequence of tokens " ++ show a]
