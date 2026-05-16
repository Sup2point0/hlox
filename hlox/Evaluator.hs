module Evaluator where

import Debug.Trace (trace)

import Evaluator.Objects qualified as Obj
import Evaluator.Objects (EvalObject, showType)
import Evaluator.Environment qualified as Env
import Evaluator.Environment (EvalEnv, ScopedEnv)
import Evaluator.Errors qualified as Err
import Evaluator.Errors (EvalError)

import Parser.Ast qualified as Ast
import Parser.Ast (Program, Node)
import Parser.Ops qualified as Op

  
type EvalResult = (EvalObject, EvalEnv)


-- | Evaluate a Lox program, given its AST.
evalProgram :: Program -> Either EvalError EvalObject
evalProgram ast = go ast Env.new
  where
    go :: Program -> EvalEnv -> Either EvalError EvalObject
    
    go [] _ = Right (Obj.Nil)
    
    go [node] env = do
      (out, _) <- eval node env
      return out
    
    go (node:nodes) env = do
      (_, env') <- eval node env
      go nodes env'
      

-- | Evaluate a node in the AST.
eval :: Node -> EvalEnv -> Either EvalError EvalResult

eval (Ast.Block nodes) env = go nodes (Env.from env)
  where
    go :: [Ast.Node] -> ScopedEnv -> Either EvalError EvalResult

    go [] env
      = Right (Obj.Nil, Env.close env)

    go (stmt:stmts) (Env.ScopedEnv env) = do
      (_, env') <- eval stmt env
      go stmts (Env.ScopedEnv env')

eval (Ast.DeclVar ident node) env = do
  (val, env') <- eval node env
  return (val, Env.define ident val env')

eval (Ast.Print node) env = do
  (node', env') <- eval node env
  return (trace ("hlox> " ++ show node') Obj.Nil, env')

eval (Ast.If cond node) env = do
  (cond', env') <- eval cond env
  case cond' of
    Obj.Boolean b -> if b then eval node env' else return (Obj.Nil, env')
    ex            -> Left (Err.TypeError ("boolean") (showType ex))

eval (Ast.IfElse cond true false) env = do
  (cond', env') <- eval cond env
  case cond' of
    Obj.Boolean b -> eval (if b then true else false) env'
    ex            -> Left (Err.TypeError ("boolean") (showType ex))


eval (Ast.AsgnVar ident node) env = do
  (val, env') <- eval node env
  env'' <- Env.set ident val env'
  return (val, env'')

eval (Ast.Var ident) env
  = case Env.get ident env of
      Just val -> return (val, env)
      Nothing  -> Left (Err.UndefinedVariable ident)

eval (Ast.Unary Op.NEGATE node) env = do
  (node', env') <- eval node env
  case node' of
    Obj.Number n -> return (Obj.Number (-n), env')
    ex           -> Left (Err.TypeError "number" (showType ex))

-- forwarding required for nice eta reduction on the rest
eval node env = eval' node env


eval' :: Node -> EvalEnv -> Either EvalError EvalResult

eval' (Ast.Stmt node) = eval node

eval' (Ast.Binary Op.EQ   left right) = evalBinaryEqOrd (==) left right
eval' (Ast.Binary Op.NEQ  left right) = evalBinaryEqOrd (==) left right
eval' (Ast.Binary Op.LT   left right) = evalBinaryEqOrd (<)  left right
eval' (Ast.Binary Op.LTEQ left right) = evalBinaryEqOrd (<=) left right
eval' (Ast.Binary Op.GT   left right) = evalBinaryEqOrd (>)  left right
eval' (Ast.Binary Op.GTEQ left right) = evalBinaryEqOrd (>=) left right

eval' (Ast.Binary Op.ADD  left right) = evalBinaryArithmetic (+) left right
eval' (Ast.Binary Op.SUB  left right) = evalBinaryArithmetic (-) left right
eval' (Ast.Binary Op.MULT left right) = evalBinaryArithmetic (*) left right
eval' (Ast.Binary Op.DIV  left right) = evalBinaryArithmetic (/) left right

eval' (Ast.Str str) = return . (Obj.String str,)
eval' (Ast.Num n)   = return . (Obj.Number n,)
eval' (Ast.Bool b)  = return . (Obj.Boolean b,)
eval' (Ast.Nil)     = return . (Obj.Nil,)
eval' node          = const  $ Left (Err.UnknownError (show node))


evalBinaryEqOrd :: (forall t. (Eq t, Ord t) => t -> t -> Bool)
                -> Node -> Node
                -> EvalEnv
                -> Either EvalError EvalResult

evalBinaryEqOrd op left right env = do
  (left' , env')  <- eval left env
  (right', env'') <- eval right env'

  case (left', right') of
    (Obj.Nil      , Obj.Nil      ) -> return (Obj.Boolean True      , env'')
    (Obj.Boolean l, Obj.Boolean r) -> return (Obj.Boolean (l `op` r), env'')
    (Obj.Number  l, Obj.Number  r) -> return (Obj.Boolean (l `op` r), env'')
    (Obj.String  l, Obj.String  r) -> return (Obj.Boolean (l `op` r), env'')
    _ -> Left (Err.MonoTypeError (showType left') (showType right'))


evalBinaryArithmetic :: (Float -> Float -> Float)
                     -> Node -> Node
                     -> EvalEnv
                     -> Either EvalError EvalResult

evalBinaryArithmetic op left right env = do
  (left',  env')  <- eval left env
  (right', env'') <- eval right env'

  case (left', right') of
    (Obj.Number l, Obj.Number r) -> return (Obj.Number (l `op` r), env'')
    (Obj.Number _, r           ) -> Left (Err.TypeError "Number" (showType r))
    (l           , Obj.Number _) -> Left (Err.TypeError "Number" (showType l))
    (l           , r           ) -> Left (Err.MonoTypeError (showType l) (showType r))
