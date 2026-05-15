module Parser.Errors where

import Lexer.Tokens (LexToken)
import Parser.Ast (Node)


data ParseError =
    UnknownError

  | InvalidAssignmentTarget Node

  | UnparsedInput [LexToken]
  | UnexpectedInput [LexToken]
  | UnexpectedEnd
  | UnexpectedToken
      LexToken          -- expected token
      (Maybe LexToken)  -- received token

  deriving (Eq)

instance Show ParseError where
  show (UnknownError)                 = "Unknown error!"
  show (InvalidAssignmentTarget node) = "Cannot assign to lvalue: " ++ show node
  show (UnparsedInput tokens)         = "Unparsed input: " ++ show tokens
  show (UnexpectedInput tokens)       = "Unexpected input: " ++ show tokens
  show (UnexpectedEnd)                = "Unexpected end of tokens!"
  show (UnexpectedToken expt recv)    = "Unexpected token: " ++ show recv ++ ", expected: " ++ show expt
