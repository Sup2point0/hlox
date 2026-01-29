module Evaluator where

import Debug.Trace (trace)

import Evaluator.Objects qualified as Obj
import Evaluator.Objects (EvalObject, showType)
import Evaluator.Errors qualified as Err
import Evaluator.Errors (EvalError)

import Parser.Ast qualified as Ast
import Parser.Ast (Program, Node)
import Parser.Ops qualified as Op


type EvalResult = Either EvalError EvalObject


evalProgram :: Program -> EvalResult
evalProgram [] = return Obj.Nil
evalProgram [node] = eval node
evalProgram (node:nodes) = do
  _ <- eval node
  evalProgram nodes


eval :: Node -> EvalResult

eval (Ast.Stmt node)  = eval node
eval (Ast.Print node) = return Obj.Nil where !_ = trace (show (eval node)) Obj.Nil

eval (Ast.Unary Op.NEGATE node) = do
  node' <- eval node
  case node' of
    Obj.Number n -> return $ Obj.Number n
    ex       -> Left (Err.TypeError "number" (showType ex))

eval (Ast.Binary Op.EQ   left right) = evalBinaryEqOrd (==) left right
eval (Ast.Binary Op.NEQ  left right) = evalBinaryEqOrd (==) left right
eval (Ast.Binary Op.LT   left right) = evalBinaryEqOrd (<)  left right
eval (Ast.Binary Op.LTEQ left right) = evalBinaryEqOrd (<=) left right
eval (Ast.Binary Op.GT   left right) = evalBinaryEqOrd (>)  left right
eval (Ast.Binary Op.GTEQ left right) = evalBinaryEqOrd (>=) left right

eval (Ast.Binary Op.ADD left right)      = evalBinaryArithmetic (+) left right
eval (Ast.Binary Op.SUBTRACT left right) = evalBinaryArithmetic (-) left right
eval (Ast.Binary Op.MULT left right)     = evalBinaryArithmetic (*) left right
eval (Ast.Binary Op.DIV left right)      = evalBinaryArithmetic (/) left right

eval (Ast.Var _)   = Left Err.UnknownError
eval (Ast.Str str) = return $ Obj.String str
eval (Ast.Num n)   = return $ Obj.Number n
eval (Ast.Bool b)  = return $ Obj.Boolean b
eval (Ast.Nil)     = return $ Obj.Nil


evalBinaryEqOrd :: (forall t. (Eq t, Ord t) => t -> t -> Bool)
                -> Node -> Node
                -> EvalResult

evalBinaryEqOrd op left right = do
  left'  <- eval left
  right' <- eval right

  case (left', right') of
    (Obj.Nil      , Obj.Nil      ) -> return $ Obj.Boolean True
    (Obj.Boolean l, Obj.Boolean r) -> return $ Obj.Boolean (l `op` r)
    (Obj.Number  l, Obj.Number  r) -> return $ Obj.Boolean (l `op` r)
    (Obj.String  l, Obj.String  r) -> return $ Obj.Boolean (l `op` r)
    _ -> Left (Err.MonoTypeError (showType left') (showType right'))


evalBinaryArithmetic :: (Float -> Float -> Float) -> Node -> Node
                     -> EvalResult

evalBinaryArithmetic op left right = do
  left'  <- eval left
  right' <- eval right

  case (left', right') of
    (Obj.Number l, Obj.Number r) -> return $ Obj.Number (l `op` r)
    (Obj.Number _, r           ) -> Left (Err.TypeError "Number" (showType r))
    (l           , Obj.Number _) -> Left (Err.TypeError "Number" (showType l))
    (l           , r           ) -> Left (Err.MonoTypeError (showType l) (showType r))
