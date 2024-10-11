module Context where

import Expr
import Parser

import qualified Data.Map as Map

data Context = Context { vars :: Map.Map String Type } deriving (Show)

-- Create an empty context
emptyContext :: Context
emptyContext = Context { vars = Map.empty }
