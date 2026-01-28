module Hlox where

import Lexer (tokenise)
import Lexer.Tokens (LexToken)
import Lexer.Errors (LexError)
import Parser (parseExpr)
import Parser.Ast (Program)
import Parser.Errors (ParseError, ParseError(UnparsedInput))
import Evaluator (eval)
import Evaluator.Objects (EvalObject)
import Evaluator.Errors (EvalError)


data Error =
    LexErr   [LexError]
  | ParseErr ParseError
  | EvalErr  EvalError
  deriving (Eq, Show)


parse :: String -> Either Error Program
parse input = do
  tokens <- tryLex input
  tryParse tokens
  where
    tryLex :: String -> Either Error [LexToken]
    tryLex src
      = case Lexer.tokenise src of
          Left err   -> Left (LexErr err)
          Right tokens -> Right tokens

    tryParse :: [LexToken] -> Either Error Program
    tryParse tokens
      = case parseExpr tokens of
          Left err          -> Left (ParseErr err)
          Right ([], ast)  -> Right ast
          Right (res, _)    -> Left (ParseErr (UnparsedInput res))

exec :: String -> Either Error EvalObject
exec src = do
  ast <- parse src
  case eval ast of
    Left err  -> Left (EvalErr err)
    Right res -> return res
