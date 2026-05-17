module Parser.Ast where

import Data.List (intercalate)
import Data.Maybe (Maybe)

import Parser.Ops


type Program = [Node]

data Node =
    Block [Node]

  -- declarations
  | DeclVar String Node
  | DeclFunc String [String] Node

  -- statements
  | Stmt Node
  | Return (Maybe Node)
  | Print Node
  | If Node Node
  | IfElse Node Node Node
  | While Node Node

  -- expressions
  | AsgnVar String Node
  | Call Node [Node]
  
  | Binary Op2 Node Node
  | Unary Op1 Node

  | Var String
  | Str String
  | Num Float
  | Bool Bool
  | Nil
  deriving Eq


instance Show Node where
  show (Block nodes) = " { " ++ (intercalate " " (map show nodes)) ++ " } "

  show (DeclVar ident node)
    = "var " ++ show ident ++ " = " ++ show node ++ ";"
  show (DeclFunc ident params body)
    = "fun " ++ (intercalate ", " (map show params)) ++ show body

  show (Stmt node)       = show node ++ ";"
  show (Return (Just r)) = "return (" ++ show r ++ ")"
  show (Return Nothing)  = "return"
  show (Print node)      = "print (" ++ show node ++ ")"
  show (If c body)       = "if (" ++ show c ++ ") " ++ show body
  show (IfElse c t f)    = "if (" ++ show c ++ ") " ++ show t ++ " else " ++ show f
  show (While c body)    = "while (" ++ show c ++ ")" ++ show body

  show (AsgnVar ident node) = show ident ++ " = " ++ show node ++ ";"
  show (Call callee args)   = show callee ++ "(" ++ (intercalate ", " (map show args)) ++ ")"

  show (Binary op l r)  = "(" ++ show l ++ show op ++ show r ++ ")"
  show (Unary op node)  = show op ++ show node

  show (Var ident) = "'" ++ ident ++ "'"
  show (Str str)   = "\"" ++ str ++ "\""
  show (Num n)     = show n
  show (Bool b)    = show b
  show  Nil        = "Nil"


child :: Node -> Maybe Node
child (DeclVar _ node) = Just node
child (Stmt node)      = Just node
child (Print node)     = Just node
child _                = Nothing
