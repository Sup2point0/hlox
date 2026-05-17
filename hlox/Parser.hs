module Parser where

import Debug.Trace

import Data.Either qualified as Either
import Data.List qualified as List
import Data.Maybe qualified as Maybe

import Parser.Ast qualified as Ast
import Parser.Ast (Program)
import Parser.Ops qualified as Op
import Parser.Ops (Op2)
import Parser.Errors qualified as Err
import Parser.Errors (ParseError)
import Parser.Helpers

import Lexer.Tokens qualified as Tk
import Lexer.Tokens (LexToken)


-- | Read a stream of tokens, producing a node `r` and returning the leftover tokens (inverted for ergonomics).
type Parser r = [LexToken] -> Either ParseError ([LexToken], r)


-- | Parse a tokenised Lox program.
parseProgram :: Parser Program
parseProgram [] = Right ([], [])
parseProgram tokens = do
  (tokens', stmt) <- parseDecl tokens
  Either.either
    (const (Left (Err.UnparsedInput tokens')))
    (\(tokens'', stmts) -> Right (tokens'', stmt:stmts))
    (parseProgram tokens')


-- | Parse a top-level declaration, the highest level in the syntax tree.
parseDecl :: Parser Ast.Node
parseDecl (Tk.VAR:ts') = parseDeclVar ts'
parseDecl tokens = parseStmt tokens


-- | Parse `var x = _`
parseDeclVar :: Parser Ast.Node

parseDeclVar ((Tk.IDENT v):(Tk.EQ):ts) = do
  (tokens', expr) <- parseExpr ts
  tokens'' <- expect Tk.SEMICOLON tokens'
  return (tokens'', Ast.DeclVar v expr)

parseDeclVar tokens = Left (Err.UnexpectedInput tokens)


-- | Parse a statement.
parseStmt :: Parser Ast.Node

parseStmt (Tk.LBRACE:ts) = do
  (tokens', stmts) <- parseBlock ts
  tokens'' <- expect Tk.RBRACE tokens'
  return (tokens'', Ast.Block stmts)

parseStmt (Tk.PRINT:ts) = do
  (tokens', expr) <- parseExpr ts
  tokens'' <- expect Tk.SEMICOLON tokens'
  return (tokens'', Ast.Print expr)

parseStmt (Tk.IF:ts) = do
  tokens1 <- expect Tk.LPAREN ts
  (tokens2, cond) <- parseExpr tokens1
  tokens3 <- expect Tk.RPAREN tokens2
  (tokens4, true) <- parseStmt tokens3
  case tokens4 of
    (Tk.ELSE:ts') -> do
      (tokens5, false) <- parseStmt ts'
      return (tokens5, Ast.IfElse cond true false)
    _
      -> return (tokens4, Ast.If cond true)

parseStmt (Tk.WHILE:ts) = do
  tokens1 <- expect Tk.LPAREN ts
  (tokens2, cond) <- parseExpr tokens1
  tokens3 <- expect Tk.RPAREN tokens2
  (tokens4, body) <- parseStmt tokens3
  return (tokens4, Ast.While cond body)

parseStmt (Tk.FOR:ts) = do
  tokens1 <- expect Tk.LPAREN ts
  
  (tokens2, init) <- case tokens1 of
    (Tk.SEMICOLON:ts') -> return (ts', Nothing)
    (Tk.VAR:ts') -> do
      (ts'', init') <- parseDeclVar ts'
      return (ts'', Just init')
    ts' -> do
      (ts'', init') <- parseStmt ts'
      return (ts'', Just init')
  
  (tokens3, cond) <- case tokens2 of
    (Tk.SEMICOLON:ts') -> return (ts', Nothing)
    ts' -> do
      (ts'', cond') <- parseExpr ts'
      ts''' <- expect Tk.SEMICOLON ts''
      return (ts''', Just cond')

  (tokens4, incr) <- case tokens3 of
    (Tk.RPAREN:ts') -> return (ts', Nothing)
    ts' -> do
      (ts'', incr') <- parseExpr ts'
      ts''' <- expect Tk.RPAREN ts''
      return (ts''', Just incr')

  (tokens5, body) <- parseStmt tokens4

  return (tokens5, Ast.Block (
      Maybe.catMaybes [
        init
      , Just $ Ast.While
          (Maybe.fromMaybe (Ast.Bool True) cond)
          (Ast.Block
            (Maybe.catMaybes [
              Just body
            , incr
            ])
          )
      ]
    ))

parseStmt tokens = do
  (tokens', expr) <- parseExpr tokens
  tokens'' <- expect Tk.SEMICOLON tokens'
  return (tokens'', Ast.Stmt expr)


-- | Parse `{ _ }`
parseBlock :: Parser [Ast.Node]
parseBlock tokens@(Tk.RBRACE:_) = return (tokens, [])
parseBlock tokens = do
  (tokens', stmt) <- parseDecl tokens
  (tokens'', stmts) <- parseBlock tokens'
  return (tokens'', stmt:stmts)


-- | Parse an expression.
parseExpr :: Parser Ast.Node
parseExpr = parseAsgn


-- | Parse `lvalue = rvalue`
parseAsgn :: Parser Ast.Node
parseAsgn tokens = do
  (tokens', lvalue) <- parseOr tokens
  parseAsgnVar lvalue tokens'


parseAsgnVar :: Ast.Node -> Parser Ast.Node

parseAsgnVar (Ast.Var v) ((Tk.EQ):ts) = do
  (tokens', value) <- parseExpr ts
  return (tokens', Ast.AsgnVar v value)

parseAsgnVar lvalue ((Tk.EQ):_)
  = Left (Err.InvalidAssignmentTarget lvalue)

parseAsgnVar value tokens = return (tokens, value)


-- | Parse `_ or _`
parseOr :: Parser Ast.Node
parseOr tokens = do
  (tokens', left) <- parseAnd tokens
  recurseBinary [(Tk.OR, Op.OR)] parseAnd left tokens'


-- | Parse `_ and _`
parseAnd :: Parser Ast.Node
parseAnd tokens = do
  (tokens', left) <- parseEquality tokens
  recurseBinary [(Tk.AND, Op.AND)] parseEquality left tokens'


-- | Parse `_ == _`
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
        (Tk.MINUS, Op.SUB) 
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
