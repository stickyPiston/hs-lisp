module Main where

import qualified Data.Map.Strict as Map
import Evaluator
import Parser
import Grammar
import Intrinsics
import System.Environment (getArgs)
import Text.ParserCombinators.Parsec (parse)

main :: IO ()
main = do
  args <- getArgs
  source <- (readFile $ args !! 0)
  case parse file (args !! 0) source of
    Right eas ->
      let pes = verifyGrammar $ map atm eas
       in case pes of
        Left es -> mapM_ putStrLn $ map ("Parsing error: " ++) es
        Right as -> do
          foldl
            (\s a -> do
               s' <- s
               if s' == Map.empty
                 then return s'
                 else do
                   t <- exec s' a
                   case t of
                     Left es -> do
                       mapM_ (putStrLn . ("ERROR: " ++)) es
                       return Map.empty
                     Right (s'', _) -> return s'')
            (pure standardScope)
            as
          return ()
    Left err -> putStrLn $ "Parsing error: " ++ show err
