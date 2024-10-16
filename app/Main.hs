module Main where

import Expr
import Typechecker
import Eval
import Context
import Parser
import Tokenizer


-- Main function to test the parser and evaluator
main :: IO ()
main = do
  putStrLn "Enter an expression to evaluate:"
  input <- getLine
  ctx <- return emptyContext
  typeCtx <- return emptyTypeContext
  if input == "exit"
    then  putStrLn "Goodbye!"
    else do case tokenize input of
              Left err  -> do
                  print err
                  main
              Right ex  -> do
                case parseExpr ex of
                  Just expr -> do
                    print expr
                    case inferType typeCtx expr of
                      Nothing -> do
                        print " failed to infer type"
                        main
                      Just t -> do
                        print t
                        main
                  _ -> do
                    putStrLn "Invalid expression"
                    main

