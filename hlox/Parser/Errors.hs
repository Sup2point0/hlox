module Parser.Errors where

import Lexer.Tokens


data ParseError =
    GeneralError
    
  | UnexpectedToken
      LexToken          -- expected token
      (Maybe LexToken)  -- received token?

  deriving (Eq, Show)
