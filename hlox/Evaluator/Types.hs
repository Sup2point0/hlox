-- Would love to separate these into files, but Haskell shrivels up and dies at even the thought of circular dependencies
module Evaluator.Types (
    EvalObject(..),
    EvalEnv(..),
    EvalError(..),
  ) where

import Data.List (intercalate)
import Data.Map (Map)

import Parser.Ast qualified as Ast
  

---------------------------------------------------------------------

data EvalObject =
    Nil
  | Boolean Bool
  | Number  Float
  | String  String

  | Callable
      String    -- identifier
      [String]  -- params
      Ast.Node  -- body (Ast.Block)
      EvalEnv   -- closure
  
  deriving (Eq)

instance Show EvalObject where
  show (Nil)        = "nil"
  show (Boolean b)  = show b
  show (Number n)   = show n
  show (String str) = "\"" ++ str ++ "\""

  show (Callable ident params _ _)
    = "<fun '" ++ ident ++ "(" ++ intercalate ", " (map show params) ++ ")'>"


---------------------------------------------------------------------

data EvalEnv = Env {
    parent :: Maybe EvalEnv
  , vars :: Map String EvalObject
  }
  deriving (Eq, Show)


---------------------------------------------------------------------

data EvalError =

  -- | (NOT AN ERROR) The current function wants to short-circuit and return a value.
    Return (EvalObject, EvalEnv)

  -- | idk what happened mate
  | UnknownError String

  -- | Tried executing an operation expecting a particular type, but received another type.
  | TypeError String String

  -- | Tried executing an operation expecting 2 objects of the same type, but received different types.
  | MonoTypeError String String

  -- | Tried reading the value of a variable which has not been declared.
  | UndefinedVariable String

instance Show EvalError where
  show (Return val)          = "Uncaught return" ++ show val
  show (UnknownError msg)    = "Unknown error: " ++ msg
  show (TypeError expt recv) = "Type error - Expected: " ++ expt ++ ", found: " ++ recv
  show (MonoTypeError l r)   = "Type error - Found incompatible types: " ++ l ++ ", " ++ r
  show (UndefinedVariable v) = "Error - Undefined variable: " ++ v

instance Eq EvalError where
  Return val1           == Return val2           = (val1 == val2)
  UnknownError msg1     == UnknownError msg2     = (msg1 == msg2)
  TypeError expt1 recv1 == TypeError expt2 recv2 = (expt1 == expt2) && (recv1 == recv2)
  MonoTypeError l1 r1   == MonoTypeError l2 r2   = (l1 == l2) && (r1 == r2)
  UndefinedVariable v1  == UndefinedVariable v2  = (v1 == v2)
  _ == _ = False
