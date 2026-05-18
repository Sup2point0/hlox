module Parser where

import Data.Either qualified as Either
import Data.List qualified as List
import Data.Maybe qualified as Maybe

import Control.Monad.State

import Parser.Ast qualified as Ast
import Parser.Ast (Program)
import Parser.Ops qualified as Op
import Parser.Ops (Op2)
import Parser.Errors qualified as Err
import Parser.Errors (ParseError)
import Parser.Helpers

import Lexer.Tokens qualified as Tk
import Lexer.Tokens (LexToken)


-- | Consume a stream of tokens, producing an output `r` and returning leftover tokens.
type Parser r = StateT [LexToken] (Either ParseError) r


-- | Try to parse a tokenised Lox program.
tryParse :: [LexToken] -> Either ParseError Program
tryParse tokens =
  case runStateT parseProgram tokens of
    Left err        -> Left err
    Right (ast, []) -> Right ast
    Right (_,  ts') -> Left (Err.UnparsedInput ts')


parseProgram :: Parser Program
parseProgram = do
  tokens <- get
  case tokens of
    [] -> return []
    _  -> do
      stmt <- parseDecl
      stmts <- parseProgram
      return (stmt:stmts)


-- | Parse a top-level declaration, the highest level in the syntax tree.
parseDecl :: Parser Ast.Node
parseDecl = do
  tokens <- get
  case tokens of
    (Tk.VAR:ts) -> put ts >> parseDeclVar
    (Tk.FUN:ts) -> put ts >> parseDeclFunc
    _ -> parseStmt


-- | Parse `var x = _`
parseDeclVar :: Parser Ast.Node
parseDeclVar = do
  tokens <- get
  case tokens of
    ((Tk.IDENT v):(Tk.EQ):ts) -> do
      put ts
      expr <- parseExpr
      expect Tk.SEMICOLON
      return (Ast.DeclVar v expr)
    _ -> get >>= lift . Left . Err.UnexpectedInput


-- | Parse `fun x() { ... }`
parseDeclFunc :: Parser Ast.Node
parseDeclFunc = do
  tokens <- get
  case tokens of
    ((Tk.IDENT f):Tk.LPAREN:ts) -> do
        put ts
        args <- case ts of
          (Tk.RPAREN:ts') -> put ts' >> return []
          _               -> parseArgs
        expect Tk.LBRACE
        body <- parseBlock
        let args' = map (\(Ast.Var ident) -> ident) args
        return (Ast.DeclFunc f args' (Ast.Block body))
    
    _ -> get >>= lift . Left . Err.UnexpectedInput


-- | Parse a statement.
parseStmt :: Parser Ast.Node

parseStmt = do
  tokens <- get
  case tokens of

    (Tk.LBRACE:ts) -> do
        put ts
        stmts <- parseBlock
        return (Ast.Block stmts)

    (Tk.RETURN:Tk.SEMICOLON:ts) -> put ts >> return (Ast.Return Nothing)

    (Tk.RETURN:ts) -> do
        put ts
        expr <- parseExpr
        expect Tk.SEMICOLON
        return (Ast.Return (Just expr))

    (Tk.PRINT:ts) -> do
        put ts
        expr <- parseExpr
        expect Tk.SEMICOLON
        return (Ast.Print expr)

    (Tk.IF:ts) -> do
        put ts
        expect Tk.LPAREN
        cond <- parseExpr
        expect Tk.RPAREN
        then' <- parseStmt

        tokens' <- get
        case tokens' of
          (Tk.ELSE:ts') -> do
              put ts'
              else' <- parseStmt
              return (Ast.IfElse cond then' else')
          _ ->
              return (Ast.If cond then')

    (Tk.WHILE:ts) -> do
        put ts
        expect Tk.LPAREN
        cond <- parseExpr
        expect Tk.RPAREN
        body <- parseStmt
        return (Ast.While cond body)

    (Tk.FOR:ts) -> do
        put ts
        expect Tk.LPAREN

        tokens' <- get
        init <- case tokens' of
          (Tk.SEMICOLON:ts') -> put ts' >> return Nothing
          _                  -> expect Tk.VAR >> parseDeclVar >>= return . Just
          -- TODO move expect VAR to parseDeclVar

        tokens'' <- get
        cond <- case tokens'' of
          (Tk.SEMICOLON:ts'') -> put ts'' >> return Nothing
          _ -> do
              cond' <- parseExpr
              expect Tk.SEMICOLON
              return (Just cond')

        tokens''' <- get
        incr <- case tokens''' of
          (Tk.RPAREN:ts''') -> put ts''' >> return Nothing
          _ -> do
            incr' <- parseExpr
            expect Tk.RPAREN
            return (Just incr')

        body <- parseStmt

        return (Ast.Block (
            Maybe.catMaybes [
              init
            , Just (Ast.While
                (Maybe.fromMaybe (Ast.Bool True) cond)
                (Ast.Block (Maybe.catMaybes [Just body, incr]))
              )
            ]
          ))

    _ -> do
        expr <- parseExpr
        expect Tk.SEMICOLON
        return (Ast.Stmt expr)


-- | Parse `{ _ }`
parseBlock :: Parser [Ast.Node]
parseBlock = do
  tokens <- get
  case tokens of
    (Tk.RBRACE:ts) -> put ts >> return []
    _ -> do
      stmt <- parseDecl
      stmts <- parseBlock
      return (stmt:stmts)


-- | Parse any expression.
parseExpr :: Parser Ast.Node
parseExpr = parseAsgn


-- | Parse `lvalue = rvalue`
parseAsgn :: Parser Ast.Node
parseAsgn = do
  lvalue <- parseOr
  parseAsgnVar lvalue

parseAsgnVar :: Ast.Node -> Parser Ast.Node
parseAsgnVar lvalue = do
  tokens <- get
  case tokens of
    (Tk.EQ:ts) -> case lvalue of
        (Ast.Var v) -> put ts >> parseExpr >>= return . Ast.AsgnVar v
        _           -> lift $ Left (Err.InvalidAssignmentTarget lvalue)
    
    _ -> return lvalue


-- | Parse `_ or _`
parseOr :: Parser Ast.Node
parseOr = do
  left <- parseAnd
  recurseBinary [(Tk.OR, Op.OR)] parseAnd left


-- | Parse `_ and _`
parseAnd :: Parser Ast.Node
parseAnd = do
  left <- parseEquality
  recurseBinary [(Tk.AND, Op.AND)] parseEquality left


-- | Parse `_ == _`
parseEquality :: Parser Ast.Node
parseEquality = do
    left <- parseComparison
    recurseBinary repl parseComparison left
  where
    repl = [
        (Tk.EQQ, Op.EQ)
      , (Tk.NEQ, Op.NEQ)
      ]


parseComparison :: Parser Ast.Node
parseComparison = do
    left <- parseTerm
    recurseBinary repl parseTerm left
  where
    repl = [
        (Tk.LT,   Op.LT)
      , (Tk.LTEQ, Op.LTEQ)
      , (Tk.GT,   Op.GT)
      , (Tk.GTEQ, Op.GTEQ)
      ]


parseTerm :: Parser Ast.Node
parseTerm = do
    left <- parseFactor
    recurseBinary repl parseFactor left
  where
    repl = [
        (Tk.PLUS, Op.ADD)
      , (Tk.MINUS, Op.SUB) 
      ]


parseFactor :: Parser Ast.Node
parseFactor = do
    left <- parseUnary
    recurseBinary repl parseUnary left
  where
    repl = [
        (Tk.STAR, Op.MULT)
      , (Tk.SLASH, Op.DIV) 
      ]


-- | Parse `-x`
parseUnary :: Parser Ast.Node
parseUnary = do
  tokens <- get
  case tokens of
    (Tk.MINUS:ts) -> put ts >> parseUnary >>= return . Ast.Unary Op.NEGATE
    _             -> parseCall


-- | Parse `x()`
parseCall :: Parser Ast.Node
parseCall = parseAtom >>= finishCall


finishCall :: Ast.Node -> Parser Ast.Node

finishCall callee = do
  tokens <- get
  case tokens of
    (Tk.LPAREN:ts) -> do
        args <- case ts of
          (Tk.RPAREN:ts') -> put ts' >> return []
          _               -> put ts  >> parseArgs
        finishCall (Ast.Call callee args)
    
    _ -> return callee


-- | Parse `x, y, ..., z)`
-- | Assumes at least 1 argument.
parseArgs :: Parser [Ast.Node]
parseArgs = do
  arg <- parseExpr

  tokens' <- get
  case tokens' of
    (Tk.RPAREN:ts') -> put ts' >> return [arg]
    (Tk.COMMA :ts') -> put ts' >> parseArgs >>= return . (arg:)
    _ -> lift $ Left (Err.UnexpectedInput tokens')


-- | Parse a primary expression.
parseAtom :: Parser Ast.Node

parseAtom = do
  tokens <- get
  case tokens of
    ((Tk.IDENT v):ts) -> put ts >> return (Ast.Var v)
    ((Tk.STR str):ts) -> put ts >> return (Ast.Str str)
    ((Tk.NUM n)  :ts) -> put ts >> return (Ast.Num n)
    ( Tk.TRUE    :ts) -> put ts >> return (Ast.Bool True)
    ( Tk.FALSE   :ts) -> put ts >> return (Ast.Bool False)
    ( Tk.NIL     :ts) -> put ts >> return (Ast.Nil)

    (Tk.LPAREN:ts) -> do
        put ts
        expr <- parseExpr
        expect Tk.RPAREN
        return expr

    [] -> lift $ Left Err.UnexpectedEnd
    _  -> lift $ Left (Err.UnexpectedInput tokens)


recurseBinary :: [(LexToken, Op2)] -> Parser Ast.Node -> Ast.Node -> Parser Ast.Node
recurseBinary repl parser left = do
  tokens <- get
  let repl' = map (\(tok, op) -> (execStateT (expect tok) tokens, op)) repl
  let continue = List.find (\(status, _) -> Either.isRight status) repl'

  case continue of
    Just (Right tokens', op) -> do
        put tokens'
        right <- parser
        let node = Ast.Binary op left right
        recurseBinary repl parser node
    
    _ -> return left

expect :: LexToken -> Parser ()
expect target = do
  tokens <- get
  let tok = safeHead tokens
  if tok == Just target then
    put $ tail tokens
  else
    lift $ Left (Err.UnexpectedToken target tok)
