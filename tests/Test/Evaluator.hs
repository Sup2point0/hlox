module Test.Evaluator where

import Data.Either qualified as Either

import Test.Tasty

import Util.Syntax

import Hlox qualified
import Parser.Ast (Program)
import Evaluator
import Evaluator.Objects qualified as Obj
import Evaluator.Environment qualified as Env
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
  , testFunctions
  , testClosures
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
    === Left (Err.UndefinedVariable "y" Env.new)
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

  , evalProgram (parse "\
      \  var a = 0;                                  \
      \  var temp = 0;                               \
      \                                              \
      \  for (var b = 1; a < 10000; b = temp + b) {  \
      \    temp = a;                                 \
      \    a = b;                                    \
      \  }                                           \
      \                                              \
      \  a;                                          \
    \")
    === Right (Obj.Number 10946)
  ]

testFunctions :: TestTree
testFunctions = testCollection "functions"
  [
    evalProgram (parse "\
      \  fun inc(n) {    \
      \      n = n + 1;  \
      \      return n;   \
      \  }               \
      \                  \
      \  inc(0);         \
    \")
    === Right (Obj.Number 1)

  , evalProgram (parse "\
      \  fun inc(n) {    \
      \      n = n + 1;  \
      \      return n;   \
      \  }               \
      \                  \
      \  inc(0);         \
    \")
    === Right (Obj.Number 1)

  , evalProgram (parse "\
      \  fun count(n) {             \
      \    while (n < 100) {        \
      \      if (n == 3) return n;  \
      \      print n;               \
      \      n = n + 1;             \
      \    }                        \
      \  }                          \
      \                             \
      \  count(1);                  \
    \")
    === Right (Obj.Number 3)

  , evalProgram (parse "\
      \  fun outer(x) {       \
      \    fun inner(x) {     \
      \      fun inc(x) {     \
      \        return x + 1;  \
      \      }                \
      \      return inc(x);   \
      \    }                  \
      \    return inner(x);   \
      \  }                    \
      \                       \
      \  outer(0);            \
    \")
    === Right (Obj.Number 1)

  , evalProgram (parse "\
      \  fun inc(n) { return n+1; }  \
      \  fun dub(n) { return 2*n; }  \
      \                              \
      \  inc(dub(7));                \
    \")
    === Right (Obj.Number 15)
  ]

testClosures :: TestTree
testClosures = testCollection "closures"
  [
    evalProgram (parse "\
      \  fun make_1() {       \
      \    fun out() {        \
      \      return 1;        \
      \    }                  \
      \                       \
      \    return out;        \
      \  }                    \
      \                       \
      \  make_1()();          \
    \")
    === Right (Obj.Number 1)

  , evalProgram (parse "\
      \  fun make_1() {       \
      \    fun out() {        \
      \      return 1;        \
      \    }                  \
      \                       \
      \    return out;        \
      \  }                    \
      \                       \
      \  var one = make_1();  \
      \  one();               \
    \")
    === Right (Obj.Number 1)

  , evalProgram (parse "\
      \  fun makeCounter() {           \
      \    var i = 0;                  \
      \                                \
      \    fun count() {               \
      \      i = i + 1;                \
      \      return i;                 \
      \    }                           \
      \                                \
      \    return count;               \
      \  }                             \
      \                                \
      \  var counter = makeCounter();  \
      \  counter();                    \
      \  counter();                    \
    \")
    === Right (Obj.Number 2)

    , evalProgram (parse "\
      \  var a = 'global';   \
      \  {                   \
      \    fun showA() {     \
      \      return a;        \
      \    }                 \
      \                      \
      \    showA();          \
      \    var a = 'block';  \
      \    showA();          \
      \  }                   \
    \")
    === Right (Obj.String "global")
  ]
