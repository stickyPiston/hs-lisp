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
  [path] <- getArgs
  ((prependedStdlib ++) -> source) <- readFile path
  case parse file path source of
    Right (filterComments -> as) ->
      foldl' (\s' a -> do
          s <- s'
          v <- runExceptT $ eval s a
          either ((>> pure empty) . putStrLn <$> ("\nRuntime Error: " ++))
            (return . fst) v
         ) (pure standardContext) as >> pure ()
    Left e -> putStrLn $ show e
  where
    filterComments = filter (not . isComment)
    isComment (Comment _) = True
    isComment _           = False
