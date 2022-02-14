{-# LANGUAGE LambdaCase #-}

module StandardContext where

import Data.Map.Strict (fromList)
import Data.List (find, intersperse)
import Control.Monad.Trans.Except
import Data.Either (lefts, isLeft, rights)
import Control.Monad.IO.Class (liftIO)

import Value

standardContext :: Context
standardContext = fromList [
    ("+", createBinop (+)),
    ("-", createBinop (-)),
    ("*", createBinop (*)),
    ("/", createBinop div),
    ("null", Intrinsic $ \case
      [ValueList l] -> return . Value.Bool $ null l
      [v] -> throwE $ "Expected List, but received " ++ typeof v
      v -> throwE $ "Expected one argument, but received " ++ show (length v)),
    ("cons₁", Intrinsic $ \case
      [e, ValueList l] -> return . ValueList $ e : l
      [_, l] -> throwE $ "Expected List, but received " ++ typeof l
      v -> throwE $ "Expected at least 2 arguments, but received " ++ show (length v)),
    ("print", Intrinsic $ \args -> do
      mapM_ (liftIO . putStrLn . show) args
      return $ ValueList args),
    ("car", Intrinsic $ \case
      [ValueList l] -> return $ head l
      [v] -> throwE $ "Expected List, but received " ++ typeof v
      v -> throwE $ "Expected 1 argument, but received " ++ show (length v)),
    ("cdr", Intrinsic $ \case
      [ValueList l] -> return . ValueList $ tail l
      [v] -> throwE $ "Expected List, but received " ++ typeof v
      v -> throwE $ "Expected 1 argument, but received " ++ show (length v)),
    ("=", Intrinsic $ \case
      [e1, e2] -> return . Bool $ e1 == e2
      v -> throwE $ "Expected 2 arguments, but received " ++ show (length v))
  ]
  where 
    createBinop :: (Int -> Int -> Int) -> Value
    createBinop f = Intrinsic $ \args ->
      let ns = map (\case
                Number n -> Right n
                v -> Left $ "Expected Number, but received " ++ typeof v) args
       in if any isLeft ns
             then throwE . concat . intersperse "\n" $ lefts ns
             else let rns = rights ns
                   in return . Number $ foldl f (head rns) (tail rns)

