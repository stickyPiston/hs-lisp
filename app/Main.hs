{-# LANGUAGE ViewPatterns #-}

module Main where

import Parser
import Evaluator
import StandardContext

import Text.ParserCombinators.Parsec (parse)
import Control.Monad.Trans.Except (runExceptT)
import System.Environment (getArgs)
import Data.Map (empty, keys)
import Data.List (foldl')

main :: IO ()
main = do
  (path : args) <- getArgs
  ((prependedStdlib ++) -> source) <- readFile path
  case parse file path source of
    Right [] -> putStrLn "Could not parse file"
    Right as -> do
      s <- foldl' (\s' a -> do
          s <- s'
          v <- runExceptT $ evalTopLevel s a
          either ((>> s') . (>> print s) . putStrLn <$> ("\nRuntime Error: " ++))
            (return . fst) v
         ) (pure standardContext) as
      (runExceptT $ eval s (Appl (Identifier "main") $ Quote $ List $ map StringLiteral args)) >>= either (putStrLn . ("ERROR: " ++)) (const $ pure ())
    Left e -> putStrLn $ show e
