module Evaluator.Objects where


data EvalObject =
    Nil
  | Boolean Bool
  | Number  Float
  | String  String
  deriving (Eq)


instance Show EvalObject where
  show (Nil)        = "nil"
  show (Boolean b)  = show b
  show (Number n)   = show n
  show (String str) = "\"" ++ str ++ "\""


showType :: EvalObject -> String
showType (Boolean _) = "Boolean"
showType (Number _)  = "Number"
showType (String _)  = "String"
showType (Nil)       = "Nil"
