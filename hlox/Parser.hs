module Parser where

import Lexer qualified
import Lexer.Tokens qualified as Tk
import Lexer.Tokens (LexToken)
import Lexer.Errors (LexError)
import Parser.Nodes qualified as Ex
import Parser.Nodes (Program, Expr)
import Parser.Ops qualified as Op
import Parser.Errors qualified as Err
import Parser.Errors (ParseError)
import Parser.Helpers


-- | Read a stream of tokens, producing a node `r` and returning the leftover tokens (inverted for ergonomics).
type Parser r = [LexToken] -> Either ParseError ([LexToken], r)


expect :: LexToken -> [LexToken] -> Either ParseError [LexToken]
expect target toks
  | tok == Just target = Right (tail toks)
  | otherwise          = Left (Err.UnexpectedToken target tok)
  where
    tok = safeHead toks


parseExpr :: Parser Expr
parseExpr = parseEquality
  where
    parseEquality :: Parser Expr
    parseEquality toks = do
      (toks', left) <- parseComparison toks

      case toks' of
        (Tk.EQQ:ts') -> do
          (toks'', right) <- parseEquality ts'
          return (toks'', Ex.Binary Op.EQ left right)
          
        (Tk.NEQ:ts') -> do
          (toks'', right) <- parseEquality ts'
          return (toks'', Ex.Binary Op.NEQ left right)
        
        _ -> return (toks', left)

    parseComparison :: Parser Expr
    parseComparison toks = do
      (toks', left) <- parseTerm toks

      case toks' of
        _ -> return (toks', left)

    parseTerm :: Parser Expr
    parseTerm toks = do
      (toks', left) <- parseFactor toks

      case toks' of
        _ -> return (toks, left)

    parseFactor :: Parser Expr
    parseFactor toks = do
      (toks', left) <- parseUnary toks

      case toks' of
        _ -> return (toks, left)

    parseUnary :: Parser Expr
    parseUnary (Tk.MINUS:ts) = do
      (toks', expr) <- parseUnary ts
      return (toks', Ex.Unary Op.NEGATE expr)
    parseUnary toks = parseAtom toks

    parseAtom :: Parser Expr
    parseAtom ((Tk.IDENT v):ts) = Right (ts, Ex.Var v)
    parseAtom ((Tk.STR str):ts) = Right (ts, Ex.Str str)
    parseAtom ((Tk.NUM n)  :ts) = Right (ts, Ex.Num n)
    parseAtom _ = Left (Err.GeneralError)


type CompileError = Either [LexError] ParseError

parse :: String -> Either CompileError Program
parse input = do
  tokens <- tryLex input
  out <- tryParse tokens
  return out
  where
    tryLex :: String -> Either CompileError [LexToken]
    tryLex src
      = case Lexer.tokenise src of
          Left err   -> Left (Left err)
          Right toks -> Right toks

    tryParse :: [LexToken] -> Either CompileError Program
    tryParse toks
      = case parseExpr toks of
          Left err          -> Left (Right err)
          Right ([], prog)  -> Right prog
          Right (res, _)    -> Left (Right (Err.UnparsedInput res))
