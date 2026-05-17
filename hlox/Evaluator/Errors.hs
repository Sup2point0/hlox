module Evaluator.Errors where

import Evaluator.Objects (EvalObject)
import {-# SOURCE #-} Evaluator.Environment (EvalEnv)


data EvalError =

  -- | (NOT AN ERROR) The current function wants to short-circuit and return a value.
    Returning (EvalObject, EvalEnv)

  -- | idk what happened mate
  | UnknownError String

  -- | Tried executing an operation expecting a particular type, but received another type.
  | TypeError String String

  -- | Tried executing an operation expecting 2 objects of the same type, but received different types.
  | MonoTypeError String String

  -- | Tried reading the value of a variable which has not been declared.
  | UndefinedVariable String


instance Show EvalError where
  show (Returning (val, _))  = "Uncaught return: " ++ show val
  show (UnknownError msg)    = "Unknown error: " ++ msg
  show (TypeError expt recv) = "Type error - Expected: " ++ expt ++ ", found: " ++ recv
  show (MonoTypeError l r)   = "Type error - Found incompatible types: " ++ l ++ ", " ++ r
  show (UndefinedVariable v) = "Error - Undefined variable: " ++ v


instance Eq EvalError where
  Returning (val1, _)   == Returning (val2, _)   = (val1 == val2)
  UnknownError msg1     == UnknownError msg2     = (msg1 == msg2)
  TypeError expt1 recv1 == TypeError expt2 recv2 = (expt1 == expt2) && (recv1 == recv2)
  MonoTypeError l1 r1   == MonoTypeError l2 r2   = (l1 == l2) && (r1 == r2)
  UndefinedVariable v1  == UndefinedVariable v2  = (v1 == v2)
