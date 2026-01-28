module Test.Parser where

import Test.Tasty

import Util.Syntax

import Lexer
import Lexer.Tokens qualified as Tk
import Parser
import Parser.Nodes qualified as Nd


testParser :: [TestTree]
testParser =
  [
    testParseAtom
  ]
  
testParseAtom :: TestTree
testParseAtom = testCollection "parseAtom"
  [
    parse "0" === Right (Nd.Num 0)
  ]
