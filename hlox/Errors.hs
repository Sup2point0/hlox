module Errors where
  
import Data.List

import Lexer.Errors (LexError)
import Parser.Errors (ParseError)
import Evaluator.Errors (EvalError)


data Error =
    LexErr   [LexError]
  | ParseErr ParseError
  | EvalErr  EvalError
  deriving Eq

instance Show Error where
  show (LexErr errs)
    = show (length errs)
      ++ " errors during lexing:\n"
      ++ intercalate "\n" (map (("  " ++) . show) errs)

  show (ParseErr err)
    = "Error during parsing:\n  " ++ show err

  show (EvalErr err)
    = "Error during evaluation:\n  " ++ show err
