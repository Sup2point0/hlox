module Parser.Nodes where

import Parser.Ops


data Expr =
    Binary Op2 Expr Expr
  | Unary Op1 Expr
  | Var String
  | Str String
  | Num Float
