module Evaluator.Types (
    EvalObject(..), EvalEnv(..)
  ) where

import Data.Map (Map)

import Parser.Ast qualified as Ast
  

data EvalObject =
    Nil
  | Boolean Bool
  | Number  Float
  | String  String

  | Callable
      String    -- identifier
      [String]  -- params
      Ast.Node  -- body
      EvalEnv   -- closure
  
  deriving (Eq)


data EvalEnv = Env {
    parent :: Maybe EvalEnv
  , vars :: Map String EvalObject
  }
  deriving (Eq)
