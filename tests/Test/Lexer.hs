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

  , tokenise "var x = 0;" === Right [Tk.VAR, Tk.IDENT "x", Tk.EQ, Tk.NUM 0, Tk.SEMICOLON]
  ]
