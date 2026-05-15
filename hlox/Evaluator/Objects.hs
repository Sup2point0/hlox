module Evaluator.Objects where


data EvalObject =
    Nil
  | Boolean Bool
  | Number  Float
  | String  String
  deriving (Eq)


instance Show EvalObject where
  show (Nil)          = "nil"
  show (Boolean bool) = show bool
  show (Number n)     = show n
  show (String s)     = "\"" ++ s ++ "\""


showType :: EvalObject -> String
showType (Boolean _) = "Boolean"
showType (Number _)  = "Number"
showType (String _)  = "String"
showType (Nil)       = "Nil"
