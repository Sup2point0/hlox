module Lexer where

import Prelude hiding (lex, pred)
import Data.Char
import Data.Either qualified as Either
import Control.Applicative

import Lexer.Tokens qualified as Tk
import Lexer.Tokens (LexToken)
import Lexer.Errors qualified as Err
import Lexer.Errors (LexError)
import Lexer.Helpers


type LexOutput = Either LexError LexToken
type LexResult = Either [LexError] [LexToken]


newtype Lexer r = Lexer {
    lex :: String -> [(r, String)]
  }
  deriving Functor

instance Applicative Lexer where
  pure :: r -> Lexer r
  pure x = Lexer (\str -> [(x, str)])
  
  liftA2 :: (r -> s -> t) -> Lexer r -> Lexer s -> Lexer t
  liftA2 f p q
    = Lexer (\str ->
        [(f r s, x') | (r, x)  <- lex p str
                     , (s, x') <- lex q x
                     ]
      )

instance Alternative Lexer where
  empty :: forall r. Lexer r
  empty = Lexer (const [])

  (<|>) :: Lexer r -> Lexer r -> Lexer r
  p <|> q = Lexer (\str -> lex p str ++ lex q str)

infixl 1 <|
(<|) :: Lexer r -> Lexer r -> Lexer r
p <| q = Lexer (\str ->
    case lex p str of
      []  -> lex q str
      res -> res
  )


satisfies :: (Char -> Bool) -> Lexer Char
satisfies pred = Lexer eat
  where
    eat :: String -> [(Char, String)]
    eat (c:cs)
      | pred c = [(c, cs)]
    eat _ = []


anyChar :: Lexer Char
anyChar = satisfies (const True)

char :: Char -> Lexer Char
char c = satisfies (== c)

string :: String -> Lexer String
string []     = pure ""
string (c:cs) = (:) <$> char c <*> string cs

letter :: Lexer Char
letter = satisfies isAlpha

digit :: Lexer Char
digit = satisfies isDigit

ignore :: Lexer Char
ignore = satisfies (\c -> c == ' ' || c == '\n')

token :: Lexer LexOutput
token =
     Right Tk.IGNORE <$ ignore
  <| Right Tk.IGNORE <$ string "//" <* many anyChar <* char '\n'

  <| Right Tk.SEMICOLON <$ char ';'
  <| Right Tk.LPAREN <$ char '('
  <| Right Tk.RPAREN <$ char ')'
  <| Right Tk.LBRACE <$ char '{'
  <| Right Tk.RBRACE <$ char '}'
  <| Right Tk.COMMA <$ char ','
  <| Right Tk.DOT   <$ char '.'
  <| Right Tk.PLUS  <$ char '+'
  <| Right Tk.MINUS <$ char '-'
  <| Right Tk.STAR  <$ char '*'
  <| Right Tk.SLASH <$ char '/'
  
  <| Right Tk.NEQ  <$ string "!="
  <| Right Tk.BANG <$ char '!'
  <| Right Tk.EQQ  <$ string "=="
  <| Right Tk.LTEQ <$ string "=<"
  <| Right Tk.EQ   <$ char '='
  <| Right Tk.GTEQ <$ string ">="
  <| Right Tk.GT   <$ char '>'
  <| Right Tk.LT   <$ char '<'

  <| Right Tk.AND    <$ string "and"
  <| Right Tk.CLASS  <$ string "class"
  <| Right Tk.ELSE   <$ string "else"
  <| Right Tk.FALSE  <$ string "false"
  <| Right Tk.FUN    <$ string "fun"
  <| Right Tk.FOR    <$ string "for"
  <| Right Tk.IF     <$ string "if"
  <| Right Tk.NIL    <$ string "nil"
  <| Right Tk.OR     <$ string "or"
  <| Right Tk.PRINT  <$ string "print"
  <| Right Tk.RETURN <$ string "return"
  <| Right Tk.SUPER  <$ string "super"
  <| Right Tk.THIS   <$ string "this"
  <| Right Tk.TRUE   <$ string "true"
  <| Right Tk.VAR    <$ string "var"
  <| Right Tk.WHILE  <$ string "while"

  <| Right . Tk.STR <$> (string "\"" *> many anyChar <* string "\"")
  <| Right . Tk.NUM . read @Float <$> ((:) <$> digit <*> many (digit <|> char '.'))
  <| Right . Tk.IDENT <$> ((:) <$> (letter <|> char '_') <*> many (letter <|> digit <|> char '_'))

  <| Left . Err.UnknownChar <$> anyChar

program :: Lexer [LexOutput]
program = many token

loxLexer :: Lexer LexResult
loxLexer = fromFallibles <$> program


tokenise :: String -> LexResult
tokenise prog
  | [] <- validLexes = head invalidLexes
  | otherwise        = cleanupLex $ head validLexes
  where
    fullLexes = [res | (res, "") <- lex loxLexer prog]
    validLexes = filter Either.isRight fullLexes
    invalidLexes = filter Either.isLeft fullLexes

    cleanupLex :: LexResult -> LexResult
    cleanupLex (Right toks) = Right (filter (not . Tk.ignored) toks)
    cleanupLex errs         = errs
