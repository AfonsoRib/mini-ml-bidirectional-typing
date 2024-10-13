module Main where

import Parser
import Expr
import Typechecker
import Eval
import Context


-- Main function to test the parser and evaluator
main :: IO ()
main = do
  putStrLn "Enter an expression to evaluate:"
  input <- getLine
  ctx <- return emptyContext
  typeCtx <- return emptyTypeContext
  if input == "exit"
    then  putStrLn "Goodbye!"
    else do case parseExpr input of
              Left err  -> do
                  print err
                  main
              Right ex  -> do
                print $ typechecker typeCtx ex
                print $ eval ctx ex
                main
