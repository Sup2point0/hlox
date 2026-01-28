module Parser.Ast where

import Parser.Ops


type Program = Expr

data Expr =
    Binary Op2 Expr Expr
  | Unary Op1 Expr
  | Var String
  | Str String
  | Num Float
  | Bool Bool
  | Nil
  deriving Eq

instance Show Expr where
  show (Binary op left right) = "(" ++ show left ++ show op ++ show right ++ ")"
  show (Unary op expr) = show op ++ show expr

  show (Var ident)     = "'" ++ show ident ++ "'"

  show (Str str) = "\"" ++ str ++ "\""
  show (Num n)   = show n
  show (Bool b)  = show b
  show  Nil      = "Nil"
