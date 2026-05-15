module Test.Parser where

import Test.Tasty

import Util.Syntax

import Hlox (parse)
import Parser.Ast
import Parser.Ops qualified as Op


testParser :: [TestTree]
testParser =
  [
    testParseAtom
  , testParseExpr
  , testParseBlock
  , testParseProgram
  ]

testParseAtom :: TestTree
testParseAtom = testCollection "parseAtom"
  [
    parse "0;" === Right [
      Stmt $ Num 0
    ]
  ]

testParseExpr :: TestTree
testParseExpr = testCollection "parseExpr"
  [
    parse "0 == 1;" === Right [
      Stmt $ Binary Op.EQ (Num 0) (Num 1)
    ]

  -- precedence
  , parse "1 + 2 == 3;"
    === Right [
      Stmt $ Binary Op.EQ
        (Binary Op.ADD (Num 1) (Num 2))
        (Num 3)
    ]

  -- associativity
  , parse "1 + 2 + 3;"
    === Right [
      Stmt $ Binary Op.ADD
        (Binary Op.ADD (Num 1) (Num 2))
        (Num 3)
    ]

  -- complex
  , parse "((6 / 3) - 1 + 10) != (-5 =< 4 + 1);"
    === Right [
      Stmt $ Binary Op.NEQ
        (Binary Op.ADD
          (Binary Op.SUBTRACT
            (Binary Op.DIV
              (Num 6)
              (Num 3)
            )
            (Num 1)
          )
          (Num 10)
        )
        (Binary Op.LTEQ
          (Unary Op.NEGATE (Num 5))
          (Binary Op.ADD (Num 4) (Num 1))
        )
    ]
  ]

testParseBlock :: TestTree
testParseBlock = testCollection "parseBlock"
  [
    parse "{}"
    === Right [
      Block []
    ]

  , parse "{ x; }"
    === Right [
      Block [
        Stmt (Var "x")
      ]
    ]

  , parse "{ var x = 0; x = x + 1; }"
    === Right [
      Block [
        DeclVar "x" (Num 0)
      , Stmt $ AsgnVar "x" (Binary Op.ADD (Var "x") (Num 1))
      ]
    ]

  , parse "{ x; } { y; }"
    === Right [
      Block [Stmt (Var "x")]
    , Block [Stmt (Var "y")]
    ]
  ]

testParseProgram :: TestTree
testParseProgram = testCollection "parseProgram"
  [
    parse "1 + 2; nil;"
    === Right [
      Stmt (
        Binary Op.ADD (Num 1.0) (Num 2.0)
      )
    , Stmt Nil
    ]
  ]
