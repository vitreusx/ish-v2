{-# LANGUAGE TupleSections #-}

module ParserDefs where

import           Lexer
import           Parser
import           Syntax
import           Layout

import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as Lex
import           Control.Monad.Combinators.Expr
import           Control.Monad                  ( void )
import           Control.Monad.State.Strict
import           Control.Lens
import           Data.Either

pTopLevel :: Parser TopLevel
pTopLevel = topLevel $ do
  sc
  r     <- ref
  items <- block ((Left <$> pStmt) <|> (Right <$> pDirective))
  eof
  return $ TopLevel r (lefts items)

reservedWords :: [String]
reservedWords =
  [ "if"
  , "else"
  , "return"
  , "continue"
  , "break"
  , "fn"
  , "for"
  , "let"
  , "in"
  , "future"
  , "rec"
  ]

reserved :: String -> Parser ()
reserved s = void $ try (symbol s)

pDirective :: Parser ()
pDirective = symbol "#" >> squareBrackets pOperatorDef

pOperatorDef :: Parser ()
pOperatorDef = do
  reserved "op"
  prec <- intLit
  op   <- pOperator
  addOperator (prec, op)

pOperator :: Parser (Operator Parser Expr)
pOperator =
  choice [try pInfixl, try pInfixn, pInfixr, pPrefix, pPostfix]

pInfixl :: Parser (Operator Parser Expr)
pInfixl = pInfix "infixl" InfixL

pInfixr :: Parser (Operator Parser Expr)
pInfixr = pInfix "infixr" InfixR

pInfixn :: Parser (Operator Parser Expr)
pInfixn = pInfix "infixn" InfixN

pInfix
  :: String
  -> (Parser (Expr -> Expr -> Expr) -> Operator Parser Expr)
  -> Parser (Operator Parser Expr)
pInfix sig ctor = do
  symbol sig
  op <- try opIdent
  return $ ctor (EInfix <$> ref <*> (Ident <$> ref <*> symbol op))

pPrefix :: Parser (Operator Parser Expr)
pPrefix = do
  symbol "prefix"
  op <- try opIdent
  return $ Prefix (EPrefix <$> ref <*> (Ident <$> ref <*> symbol op))

pPostfix :: Parser (Operator Parser Expr)
pPostfix = do
  symbol "postfix"
  op <- try opIdent
  return
    $ Postfix (EPostfix <$> ref <*> (Ident <$> ref <*> symbol op))

pIdent :: Parser Ident
pIdent = do
  r      <- ref
  ident' <- ident <|> (between (symbol "`") (symbol "`") opIdent)
  if ident' `elem` reservedWords
    then do
      fail $ "keyword " ++ (show ident') ++ " cannot be an identifier"
    else do
      return $ Ident r ident'

pContExpr :: Parser Expr
pContExpr = choice
  [ parens pExpr
  , ELitInt <$> ref <*> intLit
  , ELitStr <$> ref <*> stringLit
  , (EKind <$> ref) <* try (reserved "*")
  , (ERecur <$> ref) <* try (reserved "rec")
  , EVar <$> pIdent
  ]

pApp :: Parser Expr
pApp =
  EApp <$> ref <*> pContExpr <*> parens (pExpr `sepBy` symbol ",")

pFnArg :: Parser (Ident, Expr)
pFnArg = do
  x <- pIdent
  t <- (reserved ":" >> pExpr) <?> "type"
  return (x, t)

pFnDeclHead :: Parser ([Stmt] -> Expr)
pFnDeclHead = do
  r <- ref
  reserved "fn"
  retType <- option Nothing (Just <$> squareBrackets pExpr)
  args    <- parens (pFnArg `sepBy` symbol ",")
  return $ EFnDecl r retType args

pFnDecl :: Parser Expr
pFnDecl = withBlock' ($) pFnDeclHead pStmt

pFnType :: Parser Expr
pFnType = do
  r <- ref
  reserved "fn"
  retType  <- pContExpr
  argTypes <- parens $ pExpr `sepBy` symbol ","
  return $ EFnType r retType argTypes

pExprTerm :: Parser Expr
pExprTerm =
  try $ choice [try pFnDecl, try pFnType, try pApp, try pContExpr]

pExpr :: Parser Expr
pExpr = try $ do
  table' <- use opTable
  makeExprParser pExprTerm table'

pStmt :: Parser Stmt
pStmt = choice
  [ pLet
  , try pIfElse
  , pIf
  , pWhile
  , pBreak
  , pContinue
  , pReturn
  , pFuture
  , try pAssign
  , ExprStmt <$> (pExpr `sepBy1` symbol ",")
  ]

pLet :: Parser Stmt
pLet = withPos $ do
  r <- ref
  reserved "let"
  letItems <- pLetItem `sepBy1` symbol ","
  return $ Let r letItems

pLetItem :: Parser (Ident, Maybe Expr, Expr)
pLetItem = do
  x     <- pIdent
  xType <- option Nothing (reserved ":" >> (Just <$> (try pExpr)))
  reserved "="
  val <- pExpr
  return (x, xType, val)

pAssign :: Parser Stmt
pAssign =
  withPos $ Assign <$> ref <*> pAssignItem `sepBy1` symbol ","

pAssignItem :: Parser (Ident, Expr)
pAssignItem = do
  x <- pIdent
  reserved "="
  val <- pExpr
  return (x, val)

pIfHead :: Parser (Ref, Expr)
pIfHead = do
  r <- ref
  reserved "if"
  cond <- pExpr
  return (r, cond)

pIf :: Parser Stmt
pIf = do
  ((r, cond), body) <- withBlock (,) pIfHead pStmt
  return $ If r cond body

pElseHead :: Parser ()
pElseHead = reserved "else"

pIfElse :: Parser Stmt
pIfElse = do
  ((r, cond), then') <- withBlock (,) pIfHead pStmt
  else'              <- withBlock (flip const) pElseHead pStmt
  return $ IfElse r cond then' else'

pWhileHead :: Parser ([Stmt] -> Stmt)
pWhileHead = do
  r <- ref
  reserved "while"
  cond <- pExpr
  return $ While r cond

pWhile :: Parser Stmt
pWhile = withBlock ($) pWhileHead pStmt

pBreak :: Parser Stmt
pBreak = withPos $ do
  r <- ref
  reserved "break"
  return (Break r)

pContinue :: Parser Stmt
pContinue = withPos $ do
  r <- ref
  reserved "continue"
  return (Continue r)

pReturn :: Parser Stmt
pReturn = do
  r <- ref
  reserved "return"
  retVal <- option Nothing (Just <$> (try pExpr))
  return $ Return r retVal

pFuture :: Parser Stmt
pFuture = withPos $ do
  r <- ref
  reserved "future"
  x <- pIdent
  t <- reserved ":" >> pExpr
  return $ Future r x t
