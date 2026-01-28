module Evaluator.Objects where


data EvalObject =
    Nil
  | Boolean Bool
  | Number  Float
  | String  String
  deriving (Eq, Show)

showType :: EvalObject -> String
showType (Boolean _) = "Boolean"
showType (Number _)  = "Number"
showType (String _)  = "String"
showType (Nil)       = "Nil"
