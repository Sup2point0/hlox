module Evaluator.Objects where

import Parser.Ast qualified as Ast


data EvalObject =
    Nil
  | Boolean Bool
  | Number  Float
  | String  String

  | Callable String [String] Ast.Node
  deriving (Eq)


instance Show EvalObject where
  show (Nil)        = "nil"
  show (Boolean b)  = show b
  show (Number n)   = show n
  show (String str) = "\"" ++ str ++ "\""

  show (Callable ident _ _) = "<fun '" ++ ident ++ "'>"
  show (Callable{})         = "<invalid fun>"


showType :: EvalObject -> String
showType (Boolean _)  = "Boolean"
showType (Number _)   = "Number"
showType (String _)   = "String"
showType (Callable{}) = "Callable"
showType (Nil)        = "Nil"
