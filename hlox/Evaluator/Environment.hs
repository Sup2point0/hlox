module Evaluator.Environment where

import Data.Map qualified as Map
import Data.Map (Map)
import Data.Maybe qualified as Maybe
import Control.Applicative ((<|>))
import Control.Monad qualified as Monad

import Evaluator.Objects (EvalObject)
import Evaluator.Errors qualified as Err
import Evaluator.Errors (EvalError)


type Vars = Map String EvalObject

data EvalEnv = Env {
    parent :: Maybe EvalEnv
  , vars :: Vars
  }

-- | A scoped environment that is guaranteed (contractually) to have a parent environment.
newtype ScopedEnv = ScopedEnv EvalEnv


-- | Construct a new empty environment with no parent environment.
new :: EvalEnv
new = Env Nothing Map.empty

-- | Construct a new scoped environment under the given environment.
from :: EvalEnv -> ScopedEnv
from env = ScopedEnv (Env (Just env) Map.empty)


-- | Lookup the value of `ident` in the environment, or its parent environments if it does not exist.
get :: String -> EvalEnv -> Maybe EvalObject
get ident (Env parent vars)
  = Map.lookup ident vars
    <|> Monad.join (fmap (get ident) parent)

-- | Define `ident` to be `val` in the environment, without affecting parent environments.
define :: String -> EvalObject -> EvalEnv -> EvalEnv
define ident val (Env parent vars)
  = Env parent (Map.insert ident val vars)

-- | Set the value of `ident` to `val` in the environment, or if it not defined, the closest parent environment where it is defined.
set :: String -> EvalObject -> EvalEnv -> Either EvalError EvalEnv
set ident val (Env parent vars)
  | ident `Map.member` vars = Right $ Env parent (Map.insert ident val vars)
  | otherwise
  = case parent of
      Just p -> do
        p' <- set ident val p
        return (Env (Just p') vars)
      Nothing -> Left (Err.UndefinedVariable ident)


-- | Extract the parent environment of a scoped environment.
close :: ScopedEnv -> EvalEnv
close (ScopedEnv (Env parent _)) = Maybe.fromJust parent
