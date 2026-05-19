module Evaluator.Objects (
    module Evaluator.Objects,
    EvalObject(..)
  ) where

import Evaluator.Types (EvalObject(..))

import Parser.Ast qualified as Ast


instance Show EvalObject where
  show (Nil)        = "nil"
  show (Boolean b)  = show b
  show (Number n)   = show n
  show (String str) = "\"" ++ str ++ "\""

  show (Callable ident _ _ _) = "<fun '" ++ ident ++ "'>"
  show (Callable{})         = "<invalid fun>"


showType :: EvalObject -> String
showType (Boolean _)  = "Boolean"
showType (Number _)   = "Number"
showType (String _)   = "String"
showType (Callable{}) = "Callable"
showType (Nil)        = "Nil"
