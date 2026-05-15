module Test.Evaluator where

import Data.Either qualified as Either

import Test.Tasty

import Util.Syntax

import Hlox qualified
import Parser.Ast
import Evaluator
import Evaluator.Objects qualified as Obj
import Evaluator.Errors qualified as Err


parse :: String -> Program
parse = Either.fromRight [] . Hlox.parse


testEvaluator :: [TestTree]
testEvaluator =
  [
    testEval
  , testState
  , testStateErrors
  ]
  
testEval :: TestTree
testEval = testCollection "eval"
  [
    evalProgram (parse "1 + 2;")
    === Right (Obj.Number 3.0)

  , evalProgram (parse "1 + 2 == 3;")
    === Right (Obj.Boolean True)

  , evalProgram (parse "10 / 2 >= 5;")
    === Right (Obj.Boolean True)

  , Either.isLeft (evalProgram (parse "5 + nil != 1;"))
    === True

  , evalProgram (parse "1 + 2; nil == nil;")
    === Right (Obj.Boolean True)
  ]

testState :: TestTree
testState = testCollection "state"
  [
    evalProgram (parse "var x = 1; print x; x;")
    === Right (Obj.Number 1)

  , evalProgram (parse "var x = 1; var y = x + 1; y;")
    === Right (Obj.Number 2)

  , evalProgram (parse "var x = 1; x = x + 1; x;")
    === Right (Obj.Number 2)

  , evalProgram (parse "var x = 1; var y = 2; x = x + y; y = y + x; y;")
    === Right (Obj.Number 5)
  ]

testStateErrors :: TestTree
testStateErrors = testCollection "state"
  [
    evalProgram (parse "var x = 1; print x; y;")
    === Left (Err.UndefinedVariable "y")
  ]
