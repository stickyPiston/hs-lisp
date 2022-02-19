{-# LANGUAGE LambdaCase #-}

module StandardContext where

import Data.Map.Strict (fromList, singleton)
import Data.List (find, intersperse)
import Control.Monad.Trans.Except
import Data.Either (lefts, isLeft, rights)
import Control.Monad.IO.Class (liftIO)
import System.IO
import Control.Monad (replicateM, forM_)

import Parser hiding (Number, Bool, Char)
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
    ("read", Intrinsic $ \case
      [Handle handle, Number n] ->
        ValueList <$> map (Char) <$> (replicateM n $ liftIO (hGetChar handle))
      [Handle handle] -> return $ curryIntrinsic (Handle handle) "read"
      v -> throwE $ "Expected 2 arguments, but received " ++ show (length v)),
    ("write", Intrinsic $ \case
      [ValueList l, Handle handle] ->
        (forM_ l $ \case
          (Char c) -> liftIO $ hPutChar handle c
          v -> liftIO $ putStrLn $ typeof v) >> pure (Handle handle)
      [ValueList l] -> return $ curryIntrinsic (ValueList l) "write"
      v -> throwE $ "Expected 2 arguments, but received " ++ show (length v)),
    ("open", Intrinsic $ \case
      [Symbol m, ValueList s@(Char _ : _)] -> do
        handle <- liftIO $ openFile (map (\(Char c) -> c) s) $
          case m of
            "read" -> ReadMode
            "write" -> WriteMode
            "append" -> AppendMode
            "readwrite" -> ReadWriteMode
        return (Handle handle)
      [Symbol m] ->
        return $ curryIntrinsic (Symbol m) "open"
      v -> throwE $ "Expected 2 arguments, but received " ++ show (length v)),
    ("close", Intrinsic $ \case
      [Handle h] -> liftIO $ hClose h >> pure Nil
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
      v -> throwE $ "Expected 2 arguments, but received " ++ show (length v)),
    (">", Intrinsic $ \case
      [Number n1, Number n2] -> return . Bool $ n1 > n2
      [Number n] -> return $ curryIntrinsic (Number n) ">"
      v -> throwE $ "Expected 2 arguments, but received " ++ show (length v)),
    ("typeof", Intrinsic $ \case
      [v] -> return $ Symbol (typeof v)
      v -> throwE $ "Expected 1 argument, but received " ++ show (length v)),
    ("stdout", Handle stdout),
    ("stderr", Handle stderr),
    ("stdin",  Handle stdin)
  ]
  where 
    createBinop :: String -> (Int -> Int -> Int) -> Value
    createBinop nm f = Intrinsic $ \case
      [Number n1, Number n2] -> return . Number $ n1 `f` n2
      [Number n] -> return $ curryIntrinsic (Number n) nm
      v -> throwE $ "Expected 2 Numbers, but received " ++ show v
