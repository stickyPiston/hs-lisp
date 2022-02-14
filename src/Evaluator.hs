{-# LANGUAGE ViewPatterns #-}

module Evaluator where

import Prelude hiding (lookup)
import Data.Map.Strict hiding (foldl, filter, map)
import Data.Either (rights)
import Control.Monad.Trans.Except
import Data.List (intersperse)

import Parser
import Value

trace :: String -> Result -> Result
trace s = withExceptT (++ ("\ncalled from " ++ s))

eval :: Context -> Atom -> Result
eval s a = case a of
  Identifier "nil" -> return (s, Nil)

  Parser.Bool b -> return (s, Value.Bool b)

  StringLiteral l -> return (s, String l)

  Identifier i -> return (s, maybe Nil id $ lookup i s)

  Parser.Number n -> return (s, Value.Number n)

  Quote (List xs) ->
    traverse (eval s) xs >>=
      return . (,) s . ValueList . map snd
  Quote n@(Parser.Number _) -> eval s n
  Quote (Identifier i) -> return (s, Symbol i)
  Quote v -> throwE $ "Quoting " ++ show v ++ " is not allowed"

  List [Identifier "λ", args, body] ->
    eval s $ List [Identifier "lambda", args, body]

  List [Identifier "lambda", args, body] ->
    case args of
      -- TODO: Propagate errors from getName
      List is -> 
        let names = map getName is
         in return $ (s, Lambda s (rights names) body)
      Identifier i -> return $ (s, NaryLambda s i body)
      v -> throwE $ "Expected List or Identifier, but received " ++ show v

  List [Identifier "setq", Identifier name, expr] ->
    eval s expr >>= \(_, v) -> return (alter (maybe (Just v) =<< const) name s, Nil)

  List [Identifier "defun", name, args, body] ->
    eval s $ List [Identifier "setq", name, List $ [Identifier "lambda", args, body]]

  List [Identifier "if", cond, t, e] -> do
    (_, c) <- eval s cond
    if thruthy c
       then eval s t
       else eval s e

  List [Identifier "let", List bindings, expr] -> do
    bs <- foldl (\a b -> case b of
      List [Identifier i, e] -> do
        (s', es) <- a
        (s'', r) <- eval s' e
        return (singleton i r <> s'', es)
      v -> do
        (s', es) <- a
        return (s, ("Expected let binding, but received " ++ typeof v) : es))
        (pure (s, [])) bindings
    if length (snd bs) > 0
       then throwE . concat . intersperse "\n" $ snd bs
       else eval (fst bs) expr

  List (f : atoms) ->
    let wildcards = filter isWildcard atoms
     in if length wildcards > 0
       then
         let (_, body) = foldl (\(n, r) a ->
               if isWildcard a
                 then (n + 1, (Identifier $ "$" ++ show n) : r)
                 else (n, a : r)) (0, []) atoms
                   in eval s $ List [
                        Identifier "lambda",
                        List $ [Identifier $ "$" ++ show n | n <- [1 .. length wildcards]],
                        List $ f : body
                      ]
       else do
         -- TODO: Propagate errors from `traverse (eval s) atoms`
         let eval' = (trace (show $ List (f : atoms)) .) . eval
         (_, args) <- unzip <$> traverse (eval' s) atoms
         (_, r) <- eval' s f
         case r of
           (Lambda c ps b) ->
             let params = fromList $ zip ps args
              in eval' (unions [params, s, c]) b
           (Intrinsic f) -> f args >>= return . (,) s
           (NaryLambda c p b) ->
             let s' = insert p (ValueList args) $ c <> s
              in eval' s' b
           v -> throwE $ "Called " ++ show f ++ ", but it is not a function, but " ++ typeof v

  v -> throwE $ "Unknowing sequence of nodes: " ++ show v

  where
    isWildcard Wildcard = True
    isWildcard _        = False
    getName (Identifier n) = Right n
    getName v              = Left $ "Expected Identifier, but received " ++ show v
