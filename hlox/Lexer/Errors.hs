module Lexer.Errors where
  

data LexError =
    UnknownChar Char
    
  | GeneralError String

  deriving (Eq, Show)
