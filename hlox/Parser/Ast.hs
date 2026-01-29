module Parser.Ast where

import Parser.Ops


type Program = [Node]

data Node =
    Stmt Node
  | Print Node
  | Binary Op2 Node Node
  | Unary Op1 Node
  | Var String
  | Str String
  | Num Float
  | Bool Bool
  | Nil
  deriving Eq


instance Show Node where
  show (Stmt node)  = show node ++ ";"
  show (Print node) = "print (" ++ show node ++ ")"

  show (Binary op left right) = "(" ++ show left ++ show op ++ show right ++ ")"
  show (Unary op node) = show op ++ show node

  show (Var ident) = "'" ++ show ident ++ "'"

  show (Str str) = "\"" ++ str ++ "\""
  show (Num n)   = show n
  show (Bool b)  = show b
  show  Nil      = "Nil"


child :: Node -> Maybe Node
child (Stmt node)  = Just node
child (Print node) = Just node
child _            = Nothing
