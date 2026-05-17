module Test.Evaluator where

import Data.Either qualified as Either

import Test.Tasty

import Util.Syntax

import Hlox qualified
import Parser.Ast (Program)
import Evaluator
import Evaluator.Objects qualified as Obj
import Evaluator.Errors qualified as Err


parse :: String -> Program
parse src = Either.either
  (error . show)
  id
  (Hlox.parse src)


testEvaluator :: [TestTree]
testEvaluator =
  [
    testExpr
  , testState
  , testStateErrors
  , testScope
  , testIf
  , testLoop
  ]
  
testExpr :: TestTree
testExpr = testCollection "expr"
  [
    evalProgram (parse "nil;")
    === Right (Obj.Nil)

  , evalProgram (parse "0;")
    === Right (Obj.Number 0)

  , evalProgram (parse "true;")
    === Right (Obj.Boolean True)

  , evalProgram (parse "'sup world!';")
    === Right (Obj.String "sup world!")

  , evalProgram (parse "1 + 2;")
    === Right (Obj.Number 3)

  , evalProgram (parse "1 + 2 == 3;")
    === Right (Obj.Boolean True)

  , evalProgram (parse "1 + 2 != 3;")
    === Right (Obj.Boolean False)

  , evalProgram (parse "0 < 1;")
    === Right (Obj.Boolean True)

  , evalProgram (parse "0 > 1;")
    === Right (Obj.Boolean False)

  , evalProgram (parse "-1 > 0;")
    === Right (Obj.Boolean False)

  , evalProgram (parse "10 / 2 >= 5;")
    === Right (Obj.Boolean True)

  , evalProgram (parse "1 + 2; nil == nil;")
    === Right (Obj.Boolean True)

  , evalProgram (parse "var x = 0; x = 1;")
    === Right (Obj.Number 1)

  , evalProgram (parse "var x = 0; (x = 1) > 0;")
    === Right (Obj.Boolean True)

  , evalProgram (parse "1 == 1 and 2 == 2;")
    === Right (Obj.Boolean True)

  , evalProgram (parse "1 == 1 and 2 == 3;")
    === Right (Obj.Boolean False)

  , evalProgram (parse "1 == 1 or 2 == 3;")
    === Right (Obj.Boolean True)

  , evalProgram (parse "1 != 1 or 2 == 3;")
    === Right (Obj.Boolean False)
  ]

testExprErrors :: TestTree
testExprErrors = testCollection "expr errors"
  [
    Either.isLeft (evalProgram (parse "5 + nil != 1;"))
    === True
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
testStateErrors = testCollection "state errors"
  [
    evalProgram (parse "var x = 1; print x; y;")
    === Left (Err.UndefinedVariable "y")
  ]

testScope :: TestTree
testScope = testCollection "scope"
  [
    evalProgram (parse "\
        \  var x = 0;  \
        \  {           \
        \    x = 1;    \
        \  }           \
        \  x;          \
      \")
    === Right (Obj.Number 1)
  
  , evalProgram (parse "\
        \  var x = 0;    \
        \  {             \
        \    var x = 1;  \
        \  }             \
        \  x;            \
      \")
    === Right (Obj.Number 0)

  , evalProgram (parse "\
        \  var x = \"don't touch!\";  \
        \  {                         \
        \    var x = 'oh no';        \
        \  }                         \
        \  x;                        \
      \")
    === Right (Obj.String "don't touch!")

  , evalProgram (parse "\
        \  var y = 'do touch!';  \
        \  {                     \
        \    y = 'success!';     \
        \  }                     \
        \  y;                    \
      \")
    === Right (Obj.String "success!")

  , evalProgram (parse "\
        \  var scoped = 0;      \
        \  {                    \
        \    var scoped = 1;    \
        \    {                  \
        \      var scoped = 2;  \
        \      scoped = 3;      \
        \    }                  \
        \    scoped = 4;        \
        \  }                    \
        \  scoped;              \
      \")
    === Right (Obj.Number 0)

  , evalProgram (parse "\
        \  var a = 1;        \
        \  var r = 0;        \
        \  {                 \
        \    var a = a + 2;  \
        \    r = a;          \
        \  }                 \
        \  r;                \
      \")
    === Right (Obj.Number 3)
  ]

testIf :: TestTree
testIf = testCollection "if"
  [
    evalProgram (parse "if (true) 5;")
    === Right (Obj.Number 5)

  , evalProgram (parse "if (true) 6; else 9;")
    === Right (Obj.Number 6)

  , evalProgram (parse "if (false) 6; else 9;")
    === Right (Obj.Number 9)

  , evalProgram (parse "if (1 < 2) 6; else 9;")
    === Right (Obj.Number 6)

  , evalProgram (parse "if (1 >= 2) 6; else 9;")
    === Right (Obj.Number 9)

  , evalProgram (parse "\
        \  var x = 0;          \
        \  var r = 0;          \
        \  if ((x = 1) > 0) {  \
        \    r = 1;            \
        \  } else {            \
        \    r = 2;            \
        \  }                   \
        \  r;                  \
      \")
    === Right (Obj.Number 1)

  , evalProgram (parse "\
        \  var x = 0;          \
        \  var r = 0;          \
        \  if ((x = -1) > 0) {  \
        \    r = 1;            \
        \  } else {            \
        \    r = 2;            \
        \  }                   \
        \  r;                  \
      \")
    === Right (Obj.Number 2)
  ]

testLoop :: TestTree
testLoop = testCollection "loop"
  [
    evalProgram (parse "\
        \  var x = 0;        \
        \  while (x < 10) {  \
        \    x = x + 1;      \
        \  }                 \
        \  x;                \
      \")
    === Right (Obj.Number 10)

  , evalProgram (parse "\
        \  var p = 42;      \
        \  var q = 69;      \
        \                   \
        \  var i = p;       \
        \  var r = 0;       \
        \                   \
        \  while (i > 0) {  \
        \    r = r + q;     \
        \    i = i - 1;     \
        \  }                \
        \                   \
        \  r;               \
      \")
    === Right (Obj.Number 2898)
  ]
