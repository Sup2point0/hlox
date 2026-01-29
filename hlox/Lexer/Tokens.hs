module Lexer.Tokens where

  
data LexToken =
      IGNORE
    | EOF
    | SEMICOLON

    | LPAREN | RPAREN
    | LBRACE | RBRACE
    | COMMA | DOT | BANG
    | PLUS | MINUS | STAR | SLASH

    | NEQ
    | EQ | EQQ
    | LT | LTEQ
    | GT | GTEQ
    
    | IDENT String
    | STR String
    | NUM Float

    | AND | CLASS | ELSE | FALSE | FUN | FOR | IF | NIL | OR | PRINT | RETURN | SUPER | THIS | TRUE | VAR | WHILE
  deriving (Eq, Show)


ignored :: LexToken -> Bool
ignored = (== IGNORE)
