module Evaluator.Environment where

import Data.Map qualified as Map
import Data.Map (Map)
import Control.Applicative ((<|>))
import Control.Monad qualified as Monad

import Evaluator.Objects (EvalObject)


type Vars = Map String EvalObject

data EvalEnv = Env {
    parent :: Maybe EvalEnv
  , vars :: Vars
  }


new :: EvalEnv
new = Env Nothing Map.empty

from :: EvalEnv -> EvalEnv
from env = Env (Just env) Map.empty

get :: String -> EvalEnv -> Maybe EvalObject
get ident (Env parent vars)
  = Map.lookup ident vars
    <|> Monad.join (fmap (get ident) parent)

set :: String -> EvalObject -> EvalEnv -> EvalEnv
set ident val (Env parent vars)
  = Env parent (Map.insert ident val vars)
