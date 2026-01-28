module Test.Evaluator where

import Data.Either qualified as Either

import Test.Tasty

import Util.Syntax

import Hlox qualified
import Parser.Ast
import Evaluator
import Evaluator.Objects qualified as Obj


parse :: String -> Program
parse = Either.fromRight Nil . Hlox.parse


testEvaluator :: [TestTree]
testEvaluator =
  [
    testEval
  ]
  
testEval :: TestTree
testEval = testCollection "eval"
  [
    eval (parse "1 + 2")
    === Right (Obj.Number 3.0)

  , eval (parse "1 + 2 == 3")
    === Right (Obj.Boolean True)

  , eval (parse "10 / 2 >= 5")
    === Right (Obj.Boolean True)

  , Either.isLeft (eval (parse "5 + nil != 1"))
    === True
  ]
