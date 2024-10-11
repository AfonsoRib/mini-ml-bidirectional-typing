module Main where

import Parser
import Expr
import Typechecker
import Eval


-- Main function to test the parser and evaluator
main :: IO ()
main = do
  putStrLn "Enter an expression to evaluate:"
  input <- getLine
  if input == "exit"
    then  putStrLn "Goodbye!"
    else do case parseExpr input of
              Left err  -> do
                  print err
                  main
              Right ex  -> do
                print $ typechecker ex
                print $ eval ex
                main
