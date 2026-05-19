module Hlox (
    parse, exec,
  ) where

import Data.Bifunctor qualified as Bifunctor
import Data.Either qualified as Either

import Errors
import Lexer qualified
import Parser qualified
import Parser.Ast (Program)
import Evaluator qualified
import Evaluator.Objects (EvalObject)


-- | Try to parse the provided Lox source code into the AST of a program.
parse :: String -> Either HloxError Program
parse src = do
  tokens <- Bifunctor.first LexErr (Lexer.tokenise src)
  ast <- Bifunctor.first ParseErr (Parser.tryParse tokens)
  return ast

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
