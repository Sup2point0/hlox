module Parser.Ops where

import Prelude hiding (EQ, LT, GT)


data Op1 =
    NEGATE
  deriving Eq

data Op2 =
    EQ | NEQ
  | LT | LTEQ
  | GT | GTEQ

  | ADD | SUBTRACT
  | MULT | DIV

  deriving Eq


instance Show Op1 where
  show NEGATE = "-"

instance Show Op2 where
  show EQ   = " == "
  show NEQ  = " != "
  show LT   = " < "
  show LTEQ = " =< "
  show GT   = " > "
  show GTEQ = " >= "

  show ADD      = " + "
  show SUBTRACT = " - "
  show MULT     = " * "
  show DIV      = " / "
