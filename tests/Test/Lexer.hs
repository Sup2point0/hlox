module Test.Lexer where

import Test.Tasty

import Util.Syntax

import Lexer
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
  , tokenise "()" === Right [Tk.LPAREN, Tk.RPAREN]

  , tokenise "1 + 2" === Right [Tk.NUM 1.0, Tk.PLUS, Tk.NUM 2.0]
  , tokenise "3 == 3" === Right [Tk.NUM 3.0, Tk.EQQ, Tk.NUM 3.0]
  , tokenise "1 != 2" === Right [Tk.NUM 1.0, Tk.NEQ, Tk.NUM 2.0]
  , tokenise "1 =< 2" === Right [Tk.NUM 1.0, Tk.LTEQ, Tk.NUM 2.0]

  , tokenise "var x = 0;" === Right [Tk.VAR, Tk.IDENT "x", Tk.EQ, Tk.NUM 0, Tk.SEMICOLON]
  ]
