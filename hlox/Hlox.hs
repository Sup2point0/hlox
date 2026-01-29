module Hlox where

import Data.Either qualified as Either

import Errors
import Lexer (tokenise)
import Lexer.Tokens (LexToken)
import Parser (parseExpr)
import Parser.Ast (Program)
import Parser.Errors (ParseError(UnparsedInput))
import Evaluator (eval)
import Evaluator.Objects (EvalObject)


-- | Try to parse the provided Lox source code into the AST of a program.
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

-- | Try to execute and evaluate the provided Lox source code, crashing if the program throws an error.
exec :: String -> EvalObject
exec src = let
  parsed = parse src
  ast    = Either.fromRight (errorLeft parsed) parsed
  res    = eval ast
  in
    Either.fromRight (errorLeft res) res


errorLeft :: Show l => Either l r -> u
errorLeft = error . show . Either.fromLeft undefined
