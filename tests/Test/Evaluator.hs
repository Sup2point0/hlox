module Test.Evaluator where

import Data.Either qualified as Either

import Test.Tasty

import Util.Syntax

import Hlox qualified
import Parser.Ast
import Evaluator
import Evaluator.Objects qualified as Obj


parse :: String -> Program
parse = Either.fromRight [] . Hlox.parse


testEvaluator :: [TestTree]
testEvaluator =
  [
    testEval
  ]
  
testEval :: TestTree
testEval = testCollection "eval"
  [
    evalProgram (parse "1 + 2")
    === Right (Obj.Number 3.0)

  , evalProgram (parse "1 + 2 == 3")
    === Right (Obj.Boolean True)

  , evalProgram (parse "10 / 2 >= 5")
    === Right (Obj.Boolean True)

  , Either.isLeft (evalProgram (parse "5 + nil != 1"))
    === True

  , evalProgram (parse "1 + 2; nil == nil")
    === Right (Obj.Boolean True)
  ]
