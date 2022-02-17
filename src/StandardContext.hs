{-# LANGUAGE LambdaCase #-}

module StandardContext where

import Data.Map.Strict (fromList, singleton)
import Data.List (find, intersperse)
import Control.Monad.Trans.Except
import Data.Either (lefts, isLeft, rights)
import Control.Monad.IO.Class (liftIO)

import Parser hiding (Number, Bool)
import Value

prependedStdlib :: String
prependedStdlib = "(defun Y (f) ((λ (x) (f (λ (y) ((x x) y)))) (λ (x) (f (λ (y) ((x x) y))))))\n\n"

curryIntrinsic :: Value -> String -> Value
curryIntrinsic e nm =
  Lambda ((singleton "a" $ e) <> standardContext) "b" (List [Identifier nm, Identifier "a", Identifier "b"])

standardContext :: Context
standardContext = fromList [
    ("+", createBinop "+" (+)),
    ("-", createBinop "-" (-)),
    ("*", createBinop "*" (*)),
    ("/", createBinop "/" div),
    ("null", Intrinsic $ \case
      [ValueList l] -> return . Value.Bool $ null l
      [v] -> throwE $ "Expected List, but received " ++ show v
      v -> throwE $ "Expected one argument, but received " ++ show (length v)),
    ("cons₁", Intrinsic $ \case
      [e, ValueList l] -> return . ValueList $ e : l
      [_, l] -> throwE $ "Expected List, but received " ++ show l
      [e] -> return $ curryIntrinsic e "cons₁"
      v -> throwE $ "Expected 2 arguments, but received " ++ show (length v)),
    ("print", Intrinsic $ \case
      [v] -> (liftIO . putStrLn . show $ v) >> pure v
      v -> throwE $ "Expected 1 argument, but received " ++ show (length v)),
    ("car", Intrinsic $ \case
      [ValueList l] -> return $ head l
      [v] -> throwE $ "Expected List, but received " ++ show v
      v -> throwE $ "Expected 1 argument, but received " ++ show (length v)),
    ("cdr", Intrinsic $ \case
      [ValueList l] -> return . ValueList $ tail l
      [v] -> throwE $ "Expected List, but received " ++ show v
      v -> throwE $ "Expected 1 argument, but received " ++ show (length v)),
    ("=", Intrinsic $ \case
      [e1, e2] -> return . Bool $ e1 == e2
      [e] -> return $ curryIntrinsic e "="
      v -> throwE $ "Expected 2 arguments, but received " ++ show (length v))
  ]
  where 
    createBinop :: String -> (Int -> Int -> Int) -> Value
    createBinop nm f = Intrinsic $ \case
      [Number n1, Number n2] -> return . Number $ n1 `f` n2
      [Number n] -> return $ curryIntrinsic (Number n) nm
      v -> throwE $ "Expected 2 Numbers, but received " ++ show v
