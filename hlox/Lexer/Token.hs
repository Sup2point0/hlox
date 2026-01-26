module Lexer.Token where

  
data LexToken =
      IGNORE
    | SEMICOLON

    | LPAREN | RPAREN
    | LBRACE | RBRACE
    | COMMA | DOT | BANG
    | PLUS | MINUS | MULT | SLASH

    | NEQ
    | EQ | EQQ
    | GT | GTEQ
    | LT | LTEQ
    
    | IDENT String
    | STR String
    | NUM Float

    | AND | CLASS | ELSE | FALSE | FUN | FOR | IF | NIL | OR | PRINT | RETURN | SUPER | THIS | TRUE | VAR | WHILE
  deriving (Eq, Show)


ignored :: LexToken -> Bool
ignored = (== IGNORE)
