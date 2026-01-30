module Evaluator.Errors where


data EvalError =
    UnknownError

  -- | Tried executing an operation expecting a particular type, but received another type
  | TypeError String String

  -- | Tried executing an operation expecting 2 objects of the same type, but received different types
  | MonoTypeError String String

  -- | Tried reading the value of a variable which has not been declared
  | UndefinedVariable String
  
  deriving Eq

instance Show EvalError where
  show UnknownError                  = "Unknown error"
  show (TypeError expected received) = "Type error - Expected: " ++ expected ++ ", found: " ++ received
  show (MonoTypeError l r)           = "Type error - Found incompatible types: " ++ l ++ ", " ++ r
