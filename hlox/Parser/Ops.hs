module Parser.Ops where


data Op1 =
    NEGATE

  deriving (Eq, Show)

data Op2 =
    EQ | NEQ
  | LT | LTEQ
  | GT | GTEQ

  | PLUS | MINUS
  | TIMES | DIV

  deriving (Eq, Show)
