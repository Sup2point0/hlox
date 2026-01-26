module Lexer.Token where

  
data LexToken =
      SEMICOLON

    | LPAREN | RPAREN
    | LBRACE | RBRACE
    | COMMA | DOT | BANG
    | PLUS | MINUS | MULT | SLASH

    | NEQ
    | EQ | EQQ
    | GT | GTEQ
    | LT | LTEQ
    
    | IDENT | STR | NUM

    | AND | CLASS | ELSE | FALSE | FUN | FOR | IF | NIL | OR | PRINT | RETURN | SUPER | THIS | TRUE | VAR | WHILE

    | EOF
  deriving Eq
