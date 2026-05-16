module Parser.Ast where

import Data.List qualified as List

import Parser.Ops


type Program = [Node]

data Node =
    Block [Node]

  -- declarations
  | DeclVar String Node

  -- statements
  | Stmt Node
  | Print Node
  | If Node Node
  | IfElse Node Node Node

  -- expressions
  | AsgnVar String Node
  
  | Binary Op2 Node Node
  | Unary Op1 Node

  | Var String
  | Str String
  | Num Float
  | Bool Bool
  | Nil
  deriving Eq


instance Show Node where
  show (Block nodes)        = " { " ++ List.intercalate " " (map show nodes) ++ " } "
  show (DeclVar ident node) = "var " ++ show ident ++ " = " ++ show node ++ ";"

  show (Stmt node)    = show node ++ ";"
  show (Print node)   = "print (" ++ show node ++ ")"
  show (If c node)    = "if (" ++ show c ++ ") " ++ show node
  show (IfElse c t f) = "if (" ++ show c ++ ") " ++ show t ++ " else " ++ show f

  show (AsgnVar ident node) = show ident ++ " = " ++ show node ++ ";"

  show (Binary op left right) = "(" ++ show left ++ show op ++ show right ++ ")"
  show (Unary op node) = show op ++ show node

  show (Var ident) = "'" ++ show ident ++ "'"
  show (Str str)   = "\"" ++ str ++ "\""
  show (Num n)     = show n
  show (Bool b)    = show b
  show  Nil        = "Nil"


child :: Node -> Maybe Node
child (DeclVar _ node) = Just node
child (Stmt node)      = Just node
child (Print node)     = Just node
child _                = Nothing
