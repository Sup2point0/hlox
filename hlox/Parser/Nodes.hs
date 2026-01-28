module Parser.Nodes where

import Parser.Ops


type Program = Expr

data Expr =
    Binary Op2 Expr Expr
  | Unary Op1 Expr
  | Var String
  | Str String
  | Num Float
  deriving (Eq, Show)
