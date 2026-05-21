module Evaluator where

import Control.Monad (when)
import Control.Monad.State

import Debug.Trace (trace)

import Evaluator.Objects qualified as Obj
import Evaluator.Objects (EvalObject, showType)
import Evaluator.Environment qualified as Env
import Evaluator.Environment (EvalEnv, ScopedEnv(..))
import Evaluator.Errors qualified as Err
import Evaluator.Errors (EvalError)

import Parser.Ast qualified as Ast
import Parser.Ast (Program, Node)
import Parser.Ops qualified as Op


type EvaluatorT env res = StateT env (Either EvalError) res
type Evaluator = EvaluatorT EvalEnv EvalObject


-- | Evaluate a Lox program, given its AST.
evalProgram :: Program -> Either EvalError EvalObject
evalProgram ast = evalStateT (go ast) Env.new
  where
    -- NOTE: `[]` is not base case, but rather an exception!
    -- Programs should return their last node, only an empty program returns `Nil`.
    go :: Program -> Evaluator
    go []           = lift $ Right (Obj.Nil)
    go [node]       = eval node
    go (node:nodes) = eval node >> go nodes


-- | Evaluate a node in the AST.
eval :: Node -> Evaluator

eval (Ast.Block nodes) = do
    env <- get
    (res, env') <- lift $ runStateT (go nodes) (Env.from env)
    put $ Env.close env'
    return res
  where
    go :: [Ast.Node] -> EvaluatorT ScopedEnv EvalObject
    go [] = return Obj.Nil
    go [stmt] = do
      ScopedEnv env <- get
      (res, env') <- lift $ runStateT (eval stmt) env
      put $ ScopedEnv env'
      return res
    go (stmt:stmts) = do
      ScopedEnv env <- get
      env' <- lift $ execStateT (eval stmt) env
      put $ ScopedEnv env'
      go stmts 


eval (Ast.DeclVar ident node) = do
  val <- eval node
  val' <- case val of
    Obj.Callable _ params body cenv -> return $ Obj.Callable ident params body cenv
    _ -> return val
  
  modify $ Env.define ident val'
  return val

eval (Ast.DeclFunc ident params body) = do
  env <- get
  let func = Obj.Callable ident params body env
  modify $ Env.define ident func
  return $ trace (show env) Obj.Nil


eval (Ast.Stmt node) = eval node

eval (Ast.Return mnode) = do
  val <- case mnode of
    Just node -> eval node
    Nothing   -> lift $ Right Obj.Nil
  env <- get
  lift $ Left (Err.Return (val, env))

eval (Ast.Print node) = do
  node' <- eval node
  let traced = trace ("hlox> " ++ show node')
  return $ traced Obj.Nil

eval (Ast.If cond body) = do
  cond' <- eval cond
  case cond' of
    Obj.Boolean True  -> eval body
    Obj.Boolean False -> return Obj.Nil
    ex                -> lift $ Left (Err.TypeError "boolean" (showType ex))

eval (Ast.IfElse cond then' else') = do
  cond' <- eval cond
  case cond' of
    Obj.Boolean True  -> eval then'
    Obj.Boolean False -> eval else'
    ex                -> lift $ Left (Err.TypeError "boolean" (showType ex))

eval node@(Ast.While cond body) = do
  cond' <- eval cond
  case cond' of
    Obj.Boolean True  -> eval body >> eval node
    Obj.Boolean False -> return Obj.Nil
    ex                -> lift $ Left (Err.TypeError "boolean" (showType ex))


eval (Ast.AsgnVar ident node) = do
  val <- eval node
  val' <- case val of
    Obj.Callable _ params body cenv -> return $ Obj.Callable ident params body cenv
    _ -> return val
  
  env <- get
  env' <- lift $ Env.set ident val' env
  put env'
  return val

eval (Ast.Call callee args) = do
  callee' <- eval callee
  args' <- traverse eval args
  call callee' args'


eval (Ast.Binary Op.OR left right) = do
  left' <- eval left
  case left' of
    Obj.Boolean True  -> return left'
    Obj.Boolean False -> do
      right' <- eval right
      case right' of
        Obj.Boolean _ -> return right'
        ex            -> lift $ Left (Err.TypeError "boolean" (showType ex))
    ex -> lift $ Left (Err.TypeError "boolean" (showType ex))

eval (Ast.Binary Op.AND left right) = do
  left' <- eval left
  case left' of
    Obj.Boolean b1 -> do
      right' <- eval right
      case right' of
        Obj.Boolean b2 -> return $ Obj.Boolean (b1 && b2)
        ex             -> lift   $ Left (Err.TypeError "boolean" (showType ex))
    ex -> lift $ Left (Err.TypeError "boolean" (showType ex))


eval (Ast.Unary Op.NEGATE node) = do
  node' <- eval node
  case node' of
    Obj.Number n -> return $ Obj.Number (-n)
    ex           -> lift   $ Left (Err.TypeError "number" (showType ex))


eval (Ast.Var ident) = do
  env <- get
  case Env.get ident env of
    Just val -> return val
    Nothing  -> lift $ Left (Err.UndefinedVariable ident env)

eval (Ast.Binary Op.EQ   l r) = evalBinaryEqOrd (==) l r
eval (Ast.Binary Op.NEQ  l r) = evalBinaryEqOrd (/=) l r
eval (Ast.Binary Op.LT   l r) = evalBinaryEqOrd (<)  l r
eval (Ast.Binary Op.LTEQ l r) = evalBinaryEqOrd (<=) l r
eval (Ast.Binary Op.GT   l r) = evalBinaryEqOrd (>)  l r
eval (Ast.Binary Op.GTEQ l r) = evalBinaryEqOrd (>=) l r

eval (Ast.Binary Op.ADD  l r) = evalBinaryArithmetic (+) l r
eval (Ast.Binary Op.SUB  l r) = evalBinaryArithmetic (-) l r
eval (Ast.Binary Op.MULT l r) = evalBinaryArithmetic (*) l r
eval (Ast.Binary Op.DIV  l r) = evalBinaryArithmetic (/) l r

eval (Ast.Str str) = return $ Obj.String str
eval (Ast.Num n)   = return $ Obj.Number n
eval (Ast.Bool b)  = return $ Obj.Boolean b
eval (Ast.Nil)     = return $ Obj.Nil

-- NOTE: Yeah Haskell can prove this is unreachable, just leaving it so we get an error if we add a new AST node and haven't implemented its evaluator yet
eval node = lift $ Left (Err.UnknownError (show node))


evalBinaryEqOrd :: (forall t. (Eq t, Ord t) => t -> t -> Bool)
                -> Ast.Node -> Ast.Node
                -> Evaluator

evalBinaryEqOrd op left right = do
  left'  <- eval left
  right' <- eval right

  case (left', right') of
    (Obj.Nil      , Obj.Nil      ) -> return $ Obj.Boolean True
    (Obj.Boolean l, Obj.Boolean r) -> return $ Obj.Boolean (l `op` r)
    (Obj.Number  l, Obj.Number  r) -> return $ Obj.Boolean (l `op` r)
    (Obj.String  l, Obj.String  r) -> return $ Obj.Boolean (l `op` r)
    _ -> lift $ Left (Err.MonoTypeError (showType left') (showType right'))


evalBinaryArithmetic :: (Float -> Float -> Float)
                     -> Ast.Node -> Ast.Node
                     -> Evaluator

evalBinaryArithmetic op left right = do
  left'  <- eval left
  right' <- eval right

  case (left', right') of
    (Obj.Number l, Obj.Number r) -> return $ Obj.Number (l `op` r)
    (Obj.Number _, r           ) -> lift   $ Left (Err.TypeError "Number" (showType r))
    (l           , Obj.Number _) -> lift   $ Left (Err.TypeError "Number" (showType l))
    (l           , r           ) -> lift   $ Left (Err.MonoTypeError (showType l) (showType r))


call :: EvalObject -> [EvalObject] -> Evaluator

call (Obj.Callable ident params body cenv) args = do
  let cenv' = foldr (uncurry Env.define) cenv (zip params args)
  case runStateT (eval body) cenv' of
    Left (Err.Return (r, cenv'')) -> out cenv'' (Obj.anonymiseCallable r)
    Right            (r, cenv'')  -> out cenv'' (Obj.anonymiseCallable r)
    Left err                      -> lift $ Left err
  where
    out :: EvalEnv -> EvalObject -> Evaluator
    out cenv result = do
      let callee' = Obj.Callable ident params body cenv
      when (ident /= "") $ do
        env <- get
        env' <- lift $ Env.set ident callee' env
        put env'
      return result

call ex _ = lift $ Left (Err.TypeError "Callable" (showType ex))
