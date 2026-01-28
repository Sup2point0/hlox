module Evaluator.Errors where


data EvalError =
    UnknownError

  -- | Tried executing an operation expecting a particular type, but received another type
  | TypeError
      String   -- expected type
      String   -- received type
      -- EvalObject -- received value

  -- | Tried executing an operation expecting 2 objects of the same type, but received different types
  | MonoTypeError
      String -- left type
      String -- right type
  
  deriving (Eq, Show)
