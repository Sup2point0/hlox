module Evaluator where

import Evaluator.Objects
import Evaluator.Errors qualified as Err
import Evaluator.Errors (EvalError)

import Parser.Ast qualified as Ast
import Parser.Ast (Program, Expr)
import Parser.Ops qualified as Op


type EvalResult = Either EvalError EvalObject


eval :: Program -> EvalResult

eval (Ast.Unary Op.NEGATE expr) = do
  expr' <- eval expr
  case expr' of
    Number n -> return $ Number n
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
eval (Ast.Str str) = return $ String str
eval (Ast.Num n)   = return $ Number n
eval (Ast.Bool b)  = return $ Boolean b
eval (Ast.Nil)     = return $ Nil


evalBinaryEqOrd :: (forall t. (Eq t, Ord t) => t -> t -> Bool)
                -> Expr -> Expr
                -> EvalResult

evalBinaryEqOrd op left right = do
  left'  <- eval left
  right' <- eval right

  case (left', right') of
    (Nil      , Nil      ) -> return $ Boolean True
    (Boolean l, Boolean r) -> return $ Boolean (l `op` r)
    (Number  l, Number  r) -> return $ Boolean (l `op` r)
    (String  l, String  r) -> return $ Boolean (l `op` r)
    _ -> Left (Err.MonoTypeError (showType left') (showType right'))


evalBinaryArithmetic :: (Float -> Float -> Float) -> Expr -> Expr
                     -> EvalResult

evalBinaryArithmetic op left right = do
  left'  <- eval left
  right' <- eval right

  case (left', right') of
    (Number l, Number r) -> return $ Number (l `op` r)
    (Number _, r       ) -> Left (Err.TypeError "Number" (showType r))
    (l       , Number _) -> Left (Err.TypeError "Number" (showType l))
    (l       , r       ) -> Left (Err.MonoTypeError (showType l) (showType r))
