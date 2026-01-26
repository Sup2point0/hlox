module Lexer where

import Prelude hiding (lex)
-- import Data.Either
-- import Data.Maybe

import Lexer.Token qualified as Tk
import Lexer.Token (LexToken)
import Lexer.Helpers


data Error = Err String
data LexError = LexErr Int Error


lex :: Int -> String -> ([LexToken], [LexError])
lex _ "" = ([], [])

lex n ('/':'/':cs) = lex (n+1) $ takeUntil '\n' cs

lex n (c:cs)
  = case lexChar c cs of
      Left e          -> (   tks, e':es) where e' = LexErr n e
      Right (Just tk) -> (tk:tks,    es)
      Right _         -> (   tks,    es)
  where
    (tks, es) = lex n cs


lexChar :: Char -> [Char] -> Either Error (Maybe LexToken)

lexChar ' '  _ = Right Nothing
lexChar '\t' _ = Right Nothing
lexChar '\n' _ = Right Nothing

lexChar ';' _ = Right (Just Tk.SEMICOLON)
lexChar '(' _ = Right (Just Tk.LPAREN)
lexChar ')' _ = Right (Just Tk.RPAREN)
lexChar '{' _ = Right (Just Tk.LBRACE)
lexChar '}' _ = Right (Just Tk.RBRACE)
lexChar ',' _ = Right (Just Tk.COMMA)
lexChar '.' _ = Right (Just Tk.DOT)
lexChar '+' _ = Right (Just Tk.PLUS)
lexChar '-' _ = Right (Just Tk.MINUS)
lexChar '*' _ = Right (Just Tk.MULT)
lexChar '/' _ = Right (Just Tk.SLASH)

lexChar '!' ('=':_) = Right (Just Tk.NEQ)
lexChar '!'   _     = Right (Just Tk.BANG)
lexChar '=' ('=':_) = Right (Just Tk.EQQ)
lexChar '=' ('<':_) = Right (Just Tk.LTEQ)
lexChar '='   _     = Right (Just Tk.EQ)
lexChar '>' ('=':_) = Right (Just Tk.GTEQ)
lexChar '>'   _     = Right (Just Tk.GT)
lexChar '<'   _     = Right (Just Tk.LT)

lexChar  c  _ = Left (Err ("Unexpected character" ++ [c]))
