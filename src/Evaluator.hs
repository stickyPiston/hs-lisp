{-# LANGUAGE ViewPatterns #-}

module Evaluator where

import Prelude hiding (lookup)
import Data.Map.Strict hiding (foldl, filter, map)
import Control.Monad.Trans.Except
import Control.Monad.IO.Class (liftIO)
import Text.ParserCombinators.Parsec (parse)

import StandardContext
import Parser
import Value

evalTopLevel :: Context -> Statement -> Result
evalTopLevel s a = case a of
  Define rec name expr ->
    if rec
       then
        evalTopLevel s $ Define False name $
          Appl (Identifier "Y") $
            Abst name expr
       else do
        (_, v) <- eval s expr
        return (alter (maybe (Just v) =<< const) name s, nil)

  Import as path -> do
    ((prependedStdlib ++) -> source) <- liftIO $ readFile path
    case parse file path source of
      Right as ->
        foldl (\c a -> fst <$> (flip evalTopLevel a =<< c))
          (pure standardContext) as >>= return . flip (,) nil . (s <>)
      Left e -> throwE $ show e

eval :: Context -> Expression -> Result
eval s a = case a of
  -- Literals:

  Parser.Bool b -> return (s, Value.Bool b)
  StringLiteral l -> return (s, Value.List $ map Value.Char l)
  Identifier i -> maybe (throwE $ "Unknown variable " ++ i) (return . (,) s) $ lookup i s
  Parser.Number n -> return (s, Value.Number n)
  Comment _ -> return (s, nil)

  -- Base language constructs:

  Quote (Parser.List xs) ->
    traverse (eval s) xs >>=
      return . (,) s . Value.List . map snd
  Quote n@(Parser.Number _) -> eval s n
  Quote (Identifier i) -> return (s, Symbol i)
  Quote v -> throwE $ "Quoting " ++ show v ++ " is not allowed"

  Abst p b -> return (s, Lambda s p b)

  Let nm val expr -> do
    (_, val') <- eval s val
    eval (singleton nm val' <> s) expr

  Appl f Wildcard ->
    return (s, Lambda s "$2" $
      Abst "$1" $
        Appl (Appl f (Identifier "$1")) (Identifier "$2"))

  Appl f a -> do
    (_, rf) <- eval s f
    (_, ra) <- eval s a
    case rf of
      (Lambda c p b) -> (,) s . snd <$> eval (singleton p ra <> c) b
      (Intrinsic i) -> (,) s <$> i [ra]
      v -> throwE $ "Called " ++ show (Appl f a) ++ ", but it is not a function"

  -- Keywords:

  If cond t e -> do
    (_, c) <- trace (show cond) $ eval s cond
    if thruthy c
       then trace (show t) $ eval s t
       else trace (show e) $ eval s e

  -- No match:

  v -> throwE $ "Unknowing sequence of nodes: (" ++ typeof v ++ ")" ++ " " ++ show v

nil :: Value
nil = Value.List []

trace :: String -> Result -> Result
trace s = withExceptT (++ ("\ncalled from " ++ s))
