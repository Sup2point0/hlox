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
  , testParseIf
  , testParseWhile
  , testParseFor
  , testParseCall
  , testParseDeclFunc
  ]

testParseAtom :: TestTree
testParseAtom = testCollection "parseAtom"
  [
    parse "0;" === Right [
      Stmt $ Num 0
    ]

  , parse "-1;" === Right [
      Stmt $ Unary Op.NEGATE (Num 1)
    ]
  ]

testParseExpr :: TestTree
testParseExpr = testCollection "parseExpr"
  [
  -- single
    parse "0 == 1;" === Right [
      Stmt $ Binary Op.EQ (Num 0) (Num 1)
    ]

  , parse "-1 == -1;" === Right [
      Stmt $ Binary Op.EQ
        (Unary Op.NEGATE (Num 1))
        (Unary Op.NEGATE (Num 1))
    ]

  , parse "-1 < 0;" === Right [
      Stmt $ Binary Op.LT
        (Unary Op.NEGATE (Num 1))
        (Num 0)
    ]

  , parse "x or y;" === Right [
    Stmt $ Binary Op.OR (Var "x") (Var "y")
  ]

  , parse "x and y;" === Right [
    Stmt $ Binary Op.AND (Var "x") (Var "y")
  ]

  -- precedence
  , parse "1 + 2 == 3;"
    === Right [
      Stmt $ Binary Op.EQ
        (Binary Op.ADD (Num 1) (Num 2))
        (Num 3)
    ]

  , parse "1 == 1 and 2 != 3;"
    === Right [
      Stmt $ Binary Op.AND
        (Binary Op.EQ (Num 1) (Num 1))
        (Binary Op.NEQ (Num 2) (Num 3))
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
          (Binary Op.SUB
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

testParseIf :: TestTree
testParseIf = testCollection "if"
  [
    parse "if (true) print 1;"
    === Right [
      If (Bool True) (Print (Num 1))
    ]
  
  , parse "if (true) { print 2; }"
    === Right [
      If (Bool True) (Block [Print (Num 2)])
    ]

  , parse "if (false) print 1; else print 2;"
    === Right [
      IfElse (Bool False) (Print (Num 1)) (Print (Num 2))
    ]

  , parse "if ('first') if ('second') x; else y;"
    === Right [
      If (Str "first")
        (IfElse (Str "second")
          (Stmt (Var "x"))
          (Stmt (Var "y"))
        )
    ]
  ]

testParseBlock :: TestTree
testParseBlock = testCollection "block"
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

  , parse "x; { y; } z;"
    === Right [
      Stmt (Var "x")
    , Block [Stmt (Var "y")]
    , Stmt (Var "z")
    ]

  , parse "'1'; { '2'; } '3';"
    === Right [
      Stmt (Str "1")
    , Block [Stmt (Str "2")]
    , Stmt (Str "3")
    ]
  ]

testParseWhile :: TestTree
testParseWhile = testCollection "while"
  [
    parse "while (true) {}" === Right [
      While (Bool True) (Block [])
    ]

  , parse "while (1 == 1) print 1;" === Right [
      While
        (Binary Op.EQ (Num 1) (Num 1))
        (Print (Num 1))
    ]

  , parse "\
        \  while (x < 10) {  \
        \    x = x + 1;      \
        \    print x;        \
        \  }                 \
      \"
    === Right [
      While
        (Binary Op.LT (Var "x") (Num 10))
        (Block [
          Stmt $ AsgnVar "x" (Binary Op.ADD (Var "x") (Num 1))
        , Print (Var "x")
        ])
    ]
  ]

testParseFor :: TestTree
testParseFor = testCollection "for"
  [
    parse "for (;;) {}" === Right [
      Block [
        While (Bool True) (Block [Block []])
      ]
    ]

  , parse "for (var i = 0; i < len; i = i + 1) print i;" === Right [
    Block [
      DeclVar "i" (Num 0)
    , While
        (Binary Op.LT (Var "i") (Var "len"))
        (Block [
          Print (Var "i")
        , AsgnVar "i" (Binary Op.ADD (Var "i") (Num 1))
        ])
    ]
  ]
  ]

testParseCall :: TestTree
testParseCall = testCollection "Call"
  [
    parse "test();" === Right [
      Stmt $ Call (Var "test") []
    ]
  
  , parse "tests(1, 2, 3);" === Right [
      Stmt $ Call (Var "tests") [Num 1, Num 2, Num 3]
    ]
  
  , parse "poly()()();" === Right [
      Stmt $ Call (
        Call (
          Call (Var "poly") []
        ) []
      ) []
    ]
  
  , parse "poly(1)(2)(3);" === Right [
      Stmt $ Call (
        Call (
          Call (Var "poly") [Num 1]
        ) [Num 2]
      ) [Num 3]
    ]
  ]

testParseDeclFunc :: TestTree
testParseDeclFunc = testCollection "DeclFunc"
  [
    parse "fun test() {}" === Right [
      DeclFunc "test" [] (Block [])
    ]

  , parse "fun testing(x) { return x; }" === Right [
      DeclFunc "testing" ["x"] (Block [
        Return (Just (Var "x"))
      ])
    ]

  , parse "fun tests(x, y, z) { print x; print y; print z; }"
    === Right [
      DeclFunc "tests" ["x", "y", "z"] (Block [
        Print (Var "x")
      , Print (Var "y")
      , Print (Var "z")
      ])
    ]
  ]
