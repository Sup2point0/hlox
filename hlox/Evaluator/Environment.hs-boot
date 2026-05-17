module Evaluator.Environment where
  
import Data.Map (Map)

import Evaluator.Objects (EvalObject)


data EvalEnv = Env {
    parent :: Maybe EvalEnv
  , vars :: Map String EvalObject
  }
