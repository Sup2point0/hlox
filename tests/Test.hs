import Test.Tasty

import Test.Lexer (testLexer)
import Test.Parser (testParser)
import Test.Evaluator (testEvaluator)


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "tests"
  [
    testGroup "lexer" testLexer
  , testGroup "parser" testParser
  , testGroup "evaluator" testEvaluator
  ]
