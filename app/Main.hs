module Main where

import Expr
import Typechecker
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
                    case inferType typeCtx expr of
                      Nothing -> do
                        print expr
                        print "Either failed to infer type or not implemented"
                        main
                      Just t -> do
                        print expr
                        print t
                        main
                  _ -> do
                    putStrLn "Invalid expression"
                    main

