module Test.Lexer where

import Test.Tasty

import Util.Syntax

import Lexer
import Lexer.Tokens
import Lexer.Tokens qualified as Tk


testLexer :: [TestTree]
testLexer =
  [
    testTokenise
  ]
  
testTokenise :: TestTree
testTokenise = testCollection "tokenise"
  [
    tokenise "" === Right []
  , tokenise "()" === Right [LPAREN, RPAREN]

  , tokenise "1 + 2" === Right [NUM 1, PLUS, NUM 2]
  , tokenise "3 == 3" === Right [NUM 3, EQQ, NUM 3]
  , tokenise "1 != 2" === Right [NUM 1, NEQ, NUM 2]
  , tokenise "1 =< 2" === Right [NUM 1, LTEQ, NUM 2]

  , tokenise "var x = 0;" === Right [VAR, IDENT "x", Tk.EQ, NUM 0, SEMICOLON]

  , tokenise "for (var i = 0; i < 10; i = i + 1) {}" === Right [
    FOR, LPAREN, VAR, IDENT "i", Tk.EQ, NUM 0, SEMICOLON
  , IDENT "i", Tk.LT, NUM 10, SEMICOLON
  , IDENT "i", Tk.EQ, IDENT "i", PLUS, NUM 1, RPAREN, LBRACE, RBRACE
  ]
  ]
