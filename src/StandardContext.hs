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
prependedStdlib = "(define (Y f) ((λ (x) (f (λ (y) ((x x) y)))) (λ (x) (f (λ (y) ((x x) y))))))\n\n"

curryIntrinsic :: Value -> (Value -> Value -> ExceptT String IO Value) -> Value
curryIntrinsic e f = Intrinsic $ \[v] -> f e v

standardContext :: Context
standardContext = fromList [
    ("+", createBinop "+" (+)),
    ("-", createBinop "-" (-)),
    ("*", createBinop "*" (*)),
    ("/", createBinop "/" div),
    ("null", Intrinsic $ \case
      [Value.List l] -> return . Value.Bool $ null l
      [v] -> throwE $ "Expected List, but received " ++ show v
      v -> throwE $ "Expected one argument, but received " ++ show (length v)),
    ("cons", Intrinsic $ \case
      [e] -> return $ curryIntrinsic e
        (\e (Value.List l) -> return . Value.List $ e : l)
      [_, l] -> throwE $ "Expected List, but received " ++ show l
      v -> throwE $ "Expected 2 arguments, but received " ++ show (length v)),
    ("print", Intrinsic $ \case
      [v] -> (liftIO . putStrLn . show $ v) >> pure v
      v -> throwE $ "Expected 1 argument, but received " ++ show (length v)),
    ("read", Intrinsic $ \case
      [Number n] -> return $ curryIntrinsic (Number n)
        (\(Number n) (Handle handle) ->
          Value.List <$> map (Char) <$> (replicateM n $ liftIO (hGetChar handle)))
      v -> throwE $ "Expected 2 arguments, but received " ++ show (length v)),
    ("write", Intrinsic $ \case
      [Value.List l] -> return $ curryIntrinsic (Value.List l)
        (\(Value.List l) (Handle handle) ->
          (forM_ l $ \case
            (Char c) -> liftIO $ hPutChar handle c
            v -> liftIO $ putStrLn $ typeof v) >> pure (Handle handle))
      v -> throwE $ "Expected 2 arguments, but received " ++ show (length v)),
    ("open", Intrinsic $ \case
      [Symbol m] -> return $ curryIntrinsic (Symbol m)
        (\(Symbol m) (Value.List s@(Char _ : _)) -> do
          handle <- liftIO $ openFile (map (\(Char c) -> c) s) $
            case m of
              "read" -> ReadMode
              "write" -> WriteMode
              "append" -> AppendMode
              "readwrite" -> ReadWriteMode
          return (Handle handle))
      v -> throwE $ "Expected 2 arguments, but received " ++ show (length v)),
    ("close", Intrinsic $ \case
      [Handle h] -> liftIO $ hClose h >> pure (Value.List [])
      v -> throwE $ "Expected 1 argument, but received " ++ show (length v)),
    ("car", Intrinsic $ \case
      [Value.List l] -> return $ head l
      [v] -> throwE $ "Expected List, but received " ++ show v
      v -> throwE $ "Expected 1 argument, but received " ++ show (length v)),
    ("cdr", Intrinsic $ \case
      [Value.List l] -> return . Value.List $ tail l
      [v] -> throwE $ "Expected List, but received " ++ show v
      v -> throwE $ "Expected 1 argument, but received " ++ show (length v)),
    ("=", Intrinsic $ \case
      [e] -> return $ curryIntrinsic e
        (\e1 e2 -> return . Bool $ e1 == e2)
      v -> throwE $ "Expected 2 arguments, but received " ++ show (length v)),
    (">", Intrinsic $ \case
      [Number n] -> return $ curryIntrinsic (Number n)
        (\(Number n1) (Number n2) -> return . Bool $ n1 > n2)
      v -> throwE $ "Expected 2 arguments, but received " ++ show (length v)),
    ("typeof", Intrinsic $ \case
      [v] -> return $ Symbol (typeof v)
      v -> throwE $ "Expected 1 argument, but received " ++ show (length v)),
    ("stdout", Handle stdout),
    ("stderr", Handle stderr),
    ("stdin",  Handle stdin),
    ("num->string", Intrinsic $ \case
      [Number n] -> return $ Value.List $ map Char $ show n
      [v] -> throwE $ "Not a number!")
  ]
  where 
    createBinop :: String -> (Int -> Int -> Int) -> Value
    createBinop nm f = Intrinsic $ \case
      [Number n] -> return $ curryIntrinsic (Number n)
        (\e1 e2 -> case (e1, e2) of
                     (Number n1, Number n2) -> return . Number $ n1 `f` n2
                     _ -> throwE $ "Expected two Numbers, but received " ++ show e1 ++ " and " ++ show e2)
      v -> throwE $ "Expected a Number, but received " ++ show v
