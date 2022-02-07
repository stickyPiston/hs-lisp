{-# LANGUAGE LambdaCase #-}

module Intrinsics where

import qualified Data.Map.Strict as Map
import Data.Either (isLeft, lefts, rights, fromRight)
import Data.Char (chr, isSpace, ord)
import Parser
import Value

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
