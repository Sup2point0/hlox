module Evaluator where

import Data.Map qualified as Map
import Data.Map (Map)
-- import Data.Maybe qualified as Maybe
import Debug.Trace (trace)

import Evaluator.Objects qualified as Obj
import Evaluator.Objects (EvalObject, showType)
import Evaluator.Errors qualified as Err
import Evaluator.Errors (EvalError)

import Parser.Ast qualified as Ast
import Parser.Ast (Program, Node)
import Parser.Ops qualified as Op
import qualified Evaluator.Errors as Err


type VarsDict = Map String EvalObject
type EvalResult = (EvalObject, VarsDict)


evalProgram :: Program -> Either EvalError EvalObject
evalProgram []     = return Obj.Nil
evalProgram [node] = do
  (out, _) <- eval node Map.empty
  return out
evalProgram blocks = go blocks Map.empty
  where
    go [node] vars = do
      (out, _) <- eval node vars
      return out
    go (node:nodes) vars = do
      (_, vars') <- eval node vars
      go nodes vars'
      

eval :: Node -> VarsDict -> Either EvalError EvalResult

eval (Ast.DeclVar ident node) vars = do
  (val, vars') <- eval node vars
  return (val, Map.insert ident val vars')

eval (Ast.Print node) vars = do
  (node', vars') <- eval node vars
  return (trace (show node') Obj.Nil, vars')

eval (Ast.Var ident) vars
  = case Map.lookup ident vars of
      Just val -> return (val, vars)
      Nothing  -> Left (Err.UndefinedVariable ident)

eval (Ast.Unary Op.NEGATE node) vars = do
  (node', vars') <- eval node vars
  case node' of
    Obj.Number n -> return (Obj.Number n, vars')
    ex           -> Left (Err.TypeError "number" (showType ex))

eval node vars = eval' node vars


eval' :: Node -> VarsDict -> Either EvalError EvalResult

eval' (Ast.Stmt node)  = eval node

eval' (Ast.Binary Op.EQ   left right) = evalBinaryEqOrd (==) left right
eval' (Ast.Binary Op.NEQ  left right) = evalBinaryEqOrd (==) left right
eval' (Ast.Binary Op.LT   left right) = evalBinaryEqOrd (<)  left right
eval' (Ast.Binary Op.LTEQ left right) = evalBinaryEqOrd (<=) left right
eval' (Ast.Binary Op.GT   left right) = evalBinaryEqOrd (>)  left right
eval' (Ast.Binary Op.GTEQ left right) = evalBinaryEqOrd (>=) left right

eval' (Ast.Binary Op.ADD left right)      = evalBinaryArithmetic (+) left right
eval' (Ast.Binary Op.SUBTRACT left right) = evalBinaryArithmetic (-) left right
eval' (Ast.Binary Op.MULT left right)     = evalBinaryArithmetic (*) left right
eval' (Ast.Binary Op.DIV left right)      = evalBinaryArithmetic (/) left right

eval' (Ast.Str str) = return . (Obj.String str,)
eval' (Ast.Num n)   = return . (Obj.Number n,)
eval' (Ast.Bool b)  = return . (Obj.Boolean b,)
eval' (Ast.Nil)     = return . (Obj.Nil,)
eval' _             = const  $ Left Err.UnknownError


evalBinaryEqOrd :: (forall t. (Eq t, Ord t) => t -> t -> Bool)
                -> Node -> Node
                -> VarsDict
                -> Either EvalError EvalResult

evalBinaryEqOrd op left right vars = do
  (left' , vars')  <- eval left vars
  (right', vars'') <- eval right vars'

  case (left', right') of
    (Obj.Nil      , Obj.Nil      ) -> return (Obj.Boolean True      , vars'')
    (Obj.Boolean l, Obj.Boolean r) -> return (Obj.Boolean (l `op` r), vars'')
    (Obj.Number  l, Obj.Number  r) -> return (Obj.Boolean (l `op` r), vars'')
    (Obj.String  l, Obj.String  r) -> return (Obj.Boolean (l `op` r), vars'')
    _ -> Left (Err.MonoTypeError (showType left') (showType right'))


evalBinaryArithmetic :: (Float -> Float -> Float)
                     -> Node -> Node
                     -> VarsDict
                     -> Either EvalError EvalResult

evalBinaryArithmetic op left right vars = do
  (left',  vars')  <- eval left vars
  (right', vars'') <- eval right vars'

  case (left', right') of
    (Obj.Number l, Obj.Number r) -> return (Obj.Number (l `op` r), vars'')
    (Obj.Number _, r           ) -> Left (Err.TypeError "Number" (showType r))
    (l           , Obj.Number _) -> Left (Err.TypeError "Number" (showType l))
    (l           , r           ) -> Left (Err.MonoTypeError (showType l) (showType r))
