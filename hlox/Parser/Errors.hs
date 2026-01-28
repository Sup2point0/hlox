module Parser.Errors where

import Lexer.Tokens (LexToken)


data ParseError =
    GeneralError

  | UnparsedInput [LexToken]
    
  | UnexpectedToken
      LexToken          -- expected token
      (Maybe LexToken)  -- received token

  deriving (Eq, Show)
