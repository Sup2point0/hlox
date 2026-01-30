module Parser where

import Data.Either qualified as Either
import Data.List qualified as List
import Data.Maybe qualified as Maybe

import Parser.Ast qualified as Ast
import Parser.Ast (Program, child)
import Parser.Ops qualified as Op
import Parser.Ops (Op2)
import Parser.Errors qualified as Err
import Parser.Errors (ParseError)
import Parser.Helpers

import Lexer.Tokens qualified as Tk
import Lexer.Tokens (LexToken)


-- | Read a stream of tokens, producing a node `r` and returning the leftover tokens (inverted for ergonomics).
type Parser r = [LexToken] -> Either ParseError ([LexToken], r)


parseProgram :: Parser Program
parseProgram = parse' []
  where
    parse' :: Program -> Parser Program
    parse' acc tokens = do
      (tokens', stmt) <- parseDecl tokens
      -- Yeah pretty messy, wish GHC didn't care about indentation so much...
      let
        acc' = stmt:acc
        in case tokens' of
          [Tk.SEMICOLON] -> Right ([], reverse acc')
          []             -> Right ([], case acc' of
                                [stmt'] -> Maybe.maybeToList (child stmt')
                                _       -> reverse acc'
                              )
          (Tk.SEMICOLON:ts') -> parse' acc' ts'
          _                  -> Left (Err.UnparsedInput tokens')


parseDecl :: Parser Ast.Node
parseDecl (Tk.VAR:ts') = parseDeclVar ts'
parseDecl tokens = parseStmt tokens


parseDeclVar :: Parser Ast.Node
parseDeclVar ((Tk.IDENT v):(Tk.EQ):ts) = do
  (tokens', expr) <- parseExpr ts
  return (tokens', Ast.DeclVar v expr)
parseDeclVar tokens = Left (Err.UnexpectedInput tokens)


parseStmt :: Parser Ast.Node

parseStmt (Tk.PRINT:ts) = do
  (tokens', expr) <- parseExpr ts
  return (tokens', Ast.Print expr)

parseStmt tokens = do
  (tokens', expr) <- parseExpr tokens
  return (tokens', Ast.Stmt expr)


parseExpr :: Parser Ast.Node
parseExpr = parseEquality

parseEquality :: Parser Ast.Node
parseEquality tokens = do
    (tokens', left) <- parseComparison tokens
    recurseBinary repl parseComparison left tokens'
  where
    repl = [
        (Tk.EQQ, Op.EQ),
        (Tk.NEQ, Op.NEQ)
      ]

parseComparison :: Parser Ast.Node
parseComparison tokens = do
    (tokens', left) <- parseTerm tokens
    recurseBinary repl parseTerm left tokens'
  where
    repl = [
        (Tk.LT,   Op.LT),
        (Tk.LTEQ, Op.LTEQ),
        (Tk.GT,   Op.GT),
        (Tk.GTEQ, Op.GTEQ)
      ]

parseTerm :: Parser Ast.Node
parseTerm tokens = do
    (tokens', left) <- parseFactor tokens
    recurseBinary repl parseFactor left tokens'
  where
    repl = [
        (Tk.PLUS, Op.ADD),
        (Tk.MINUS, Op.SUBTRACT) 
      ]

parseFactor :: Parser Ast.Node
parseFactor tokens = do
    (tokens', left) <- parseUnary tokens
    recurseBinary repl parseUnary left tokens'
  where
    repl = [
        (Tk.STAR, Op.MULT),
        (Tk.SLASH, Op.DIV) 
      ]

parseUnary :: Parser Ast.Node
parseUnary (Tk.MINUS:ts) = do
  (tokens', expr) <- parseUnary ts
  return (tokens', Ast.Unary Op.NEGATE expr)
parseUnary tokens = parseAtom tokens


parseAtom :: Parser Ast.Node

parseAtom ((Tk.IDENT v):ts) = Right (ts, Ast.Var v)
parseAtom ((Tk.STR str):ts) = Right (ts, Ast.Str str)

parseAtom ((Tk.NUM n):ts) = Right (ts, Ast.Num n)
parseAtom ((Tk.TRUE) :ts) = Right (ts, Ast.Bool True)
parseAtom ((Tk.FALSE):ts) = Right (ts, Ast.Bool False)
parseAtom ((Tk.NIL)  :ts) = Right (ts, Ast.Nil)

parseAtom ((Tk.LPAREN):ts) = do
  (tokens', expr) <- parseExpr ts
  tokens'' <- expect Tk.RPAREN tokens'
  return (tokens'', expr)

parseAtom []     = Left Err.UnexpectedEnd
parseAtom tokens = Left (Err.UnexpectedInput tokens)


recurseBinary :: [(LexToken, Op2)] -> Parser Ast.Node -> Ast.Node -> Parser Ast.Node
recurseBinary repl parser left tokens
  = case continue of
      Just (Right tokens', op) -> do
        (tokens'', right) <- parser tokens'

        let node = Ast.Binary op left right
          in recurseBinary repl parser node tokens''
      
      _ -> return (tokens, left)
  where
    repl' :: [(Either ParseError [LexToken], Op2)]
    repl' = map (\(tok, op) -> (expect tok tokens, op)) repl

    continue :: Maybe (Either ParseError [LexToken], Op2)
    continue = List.find (\(tokens', _) -> Either.isRight tokens') repl'

expect :: LexToken -> [LexToken] -> Either ParseError [LexToken]
expect target tokens
  | tok == Just target = Right (tail tokens)
  | otherwise          = Left (Err.UnexpectedToken target tok)
  where
    tok = safeHead tokens
