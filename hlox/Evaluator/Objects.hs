module Evaluator.Objects (
    module Evaluator.Objects,
    EvalObject(..)
  ) where

import Evaluator.Types (EvalObject(..))


showType :: EvalObject -> String
showType (Boolean _)  = "Boolean"
showType (Number _)   = "Number"
showType (String _)   = "String"
showType (Callable{}) = "Callable"
showType (Nil)        = "Nil"
