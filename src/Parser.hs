{-# LANGUAGE TupleSections #-}

module Parser where

import           Lexer
import           ParserDef
import           Syntax
import           Ontology
import           Indent
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as Lex
import           Control.Monad.Combinators.Expr
import           Control.Monad                  ( void )
import           Control.Monad.State.Strict
import           Control.Lens
import           Data.Either

ref :: Parser Anchor
ref = getOffset

pTopLevel :: Parser TopLevel
pTopLevel = topLevel $ do
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
  op <- opIdent
  return $ ctor (EInfix <$> ref <*> (Ident <$> ref <*> symbol op))

pPrefix :: Parser (Operator Parser Expr)
pPrefix = do
  symbol "prefix"
  op <- opIdent
  return $ Prefix (EPrefix <$> ref <*> (Ident <$> ref <*> symbol op))

pPostfix :: Parser (Operator Parser Expr)
pPostfix = do
  symbol "postfix"
  op <- opIdent
  return
    $ Postfix (EPostfix <$> ref <*> (Ident <$> ref <*> symbol op))

pIdent :: Parser Ident
pIdent = try $ do
  r      <- ref
  ident' <- ident
  if ident' `elem` reservedWords
    then do
      fail $ "keyword " ++ (show ident') ++ " cannot be an identifier"
    else do
      return $ Ident r ident'

pValue :: Parser Value
pValue = choice
  [try (IntV <$> intLit), RealV <$> realLit, StringV <$> stringLit]

pFnArg :: Parser (Ident, Typename)
pFnArg = do
  x <- pIdent
  t <- (reserved ":" >> pTypename) <?> "type"
  return (x, t)

pFnDeclHead :: Parser ([Stmt] -> Expr)
pFnDeclHead = do
  r <- ref
  reserved "fn"
  retType <- option Nothing (Just <$> (try pTypename))
  args    <- parens (pFnArg `sepBy` symbol ",")
  return $ EFnDecl r retType args

pFnDecl :: Parser Expr
pFnDecl = withBlock' ($) pFnDeclHead pStmt

pApp :: Parser Expr
pApp =
  EApp <$> ref <*> pContExpr <*> parens (pExpr `sepBy` symbol ",")

pContExpr :: Parser Expr
pContExpr = choice
  [parens pExpr, ELit <$> ref <*> pValue, EVar <$> ref <*> pIdent]

pExprTerm :: Parser Expr
pExprTerm = choice [try pApp, pContExpr, try pFnDecl]

pExpr :: Parser Expr
pExpr = use opTable >>= makeExprParser pExprTerm

pContTypename :: Parser Typename
pContTypename = choice [parens pTypename, Tyvar <$> ref <*> pIdent]

pFnTypename :: Parser Typename
pFnTypename = do
  r <- ref
  reserved "fn"
  retType  <- pContTypename
  argTypes <- parens $ pTypename `sepBy` symbol ","
  return $ FnTypename r retType argTypes

pTypename :: Parser Typename
pTypename = choice [try pContTypename, pFnTypename]

pStmt :: Parser Stmt
pStmt = choice
  [ pLet
  , try pAssign
  , try pIfElse
  , pIf
  , pFor
  , pWhile
  , pBreak
  , pContinue
  , pReturn
  , ExprStmt <$> ref <*> (pExpr `sepBy1` symbol ",")
  ]

pLet :: Parser Stmt
pLet = withPos $ do
  r <- ref
  reserved "let"
  letItems <- pLetItem `sepBy1` symbol ","
  return $ Let r letItems

pLetItem :: Parser (Ident, Maybe Typename, Expr)
pLetItem = do
  x     <- pIdent
  xType <- option Nothing (reserved ":" >> (Just <$> (try pTypename)))
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

pIfHead :: Parser (Anchor, Expr)
pIfHead = do
  r <- ref
  reserved "if"
  cond <- parens pExpr
  return (r, cond)

pIf :: Parser Stmt
pIf = do
  ((r, cond), body) <- withBlock (,) pIfHead pStmt
  return $ If r cond body

pElseHead :: Parser ()
pElseHead = void $ reserved "else"

pIfElse :: Parser Stmt
pIfElse = do
  ((r, cond), then') <- withBlock (,) pIfHead pStmt
  else'              <- withBlock (flip const) pElseHead pStmt
  return $ IfElse r cond then' else'

pForHead :: Parser ([Stmt] -> Stmt)
pForHead = do
  r <- ref
  reserved "for"
  (x, seq) <- parens $ (,) <$> pIdent <* reserved "in" <*> pExpr
  return $ For r x seq

pFor :: Parser Stmt
pFor = withBlock ($) pForHead pStmt

pWhileHead :: Parser ([Stmt] -> Stmt)
pWhileHead = do
  r <- ref
  reserved "while"
  cond <- parens pExpr
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
pReturn = withPos $ do
  r <- ref
  reserved "return"
  retVal <- option Nothing (Just <$> (try pExpr))
  return $ Return r retVal
