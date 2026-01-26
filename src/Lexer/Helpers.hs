module Lexer.Helpers where


takeUntil :: Char -> String -> String
takeUntil _ "" = ""
takeUntil t (c:cs)
  | t == c    = cs
  | otherwise = takeUntil t cs
