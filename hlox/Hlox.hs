module Hlox where

import Data.Either qualified as Either

import Errors
import Lexer qualified
import Lexer.Tokens (LexToken)
import Parser qualified
import Parser.Ast (Program)
import Parser.Errors (ParseError(UnparsedInput))
import Evaluator qualified
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
          Left err     -> Left (LexErr err)
          Right tokens -> Right tokens

    tryParse :: [LexToken] -> Either Error Program
    tryParse tokens
      = case Parser.parseProgram tokens of
          Left err        -> Left (ParseErr err)
          Right ([], ast) -> Right ast
          Right (res, _)  -> Left (ParseErr (UnparsedInput res))

-- | Try to execute and evaluate the provided Lox source code, crashing if the program throws an error.
exec :: String -> EvalObject
exec src = let
  parsed = parse src
  ast    = Either.fromRight (errorLeft parsed) parsed
  res    = Evaluator.evalProgram ast
  in
    Either.fromRight (errorLeft res) res


errorLeft :: Show l => Either l r -> u
errorLeft = error . show . Either.fromLeft undefined
