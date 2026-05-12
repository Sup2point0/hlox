module Parser.Errors where

import Lexer.Tokens (LexToken)
import Parser.Ast (Node)


data ParseError =
    GeneralError

  | InvalidAssignmentTarget Node

  | UnparsedInput [LexToken]
  | UnexpectedInput [LexToken]
  | UnexpectedEnd
  | UnexpectedToken
      LexToken          -- expected token
      (Maybe LexToken)  -- received token

  deriving (Eq, Show)
