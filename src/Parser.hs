{-# LANGUAGE TupleSections #-}

module Parser where

import           Lexer
import           ParserDef
import           Syntax
import           Value
import           Indent
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as Lex
import           Control.Monad.Combinators.Expr
import           Control.Monad                  ( void )
import           Control.Monad.State.Strict
import           Control.Lens
import           Data.Either

sp :: Parser SourcePos
sp = getSourcePos

pTopLevel :: Parser TopLevel
pTopLevel = topLevel $ do
  s     <- sp
  items <- block ((Left <$> pStmt) <|> (Right <$> pDirective))
  eof
  return $ TopLevel s (lefts items)

reservedWords :: [String]
reservedWords =
  [ "if"
  , "then"
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
reserved s = do
  try (symbol s)
  return ()

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
  return $ ctor (EInfix <$> sp <*> (Ident <$> sp <*> symbol op))

pPrefix :: Parser (Operator Parser Expr)
pPrefix = do
  symbol "prefix"
  op <- opIdent
  return $ Prefix (EPrefix <$> sp <*> (Ident <$> sp <*> symbol op))

pPostfix :: Parser (Operator Parser Expr)
pPostfix = do
  symbol "postfix"
  op <- opIdent
  return $ Postfix (EPostfix <$> sp <*> (Ident <$> sp <*> symbol op))

pIdent :: Parser Ident
pIdent = try $ do
  s      <- sp
  ident' <- ident
  if ident' `elem` reservedWords
    then do
      fail $ "keyword " ++ (show ident') ++ " cannot be an identifier"
    else do
      return $ Ident s ident'

pValue :: Parser Value
pValue = choice
  [try (IntV <$> intLit), RealV <$> realLit, StringV <$> stringLit]

pFnArg :: Parser (Ident, Type)
pFnArg = do
  x <- pIdent
  t <- (reserved ":" >> pType) <?> "type"
  return (x, t)

pFnDeclHead :: Parser ([Stmt] -> Expr)
pFnDeclHead = do
  p <- sp
  reserved "fn"
  retType <- option Nothing (Just <$> (try pType))
  args    <- parens (pFnArg `sepBy` symbol ",")
  return $ EFnDecl p retType args

pFnDecl :: Parser Expr
pFnDecl = withBlock' ($) pFnDeclHead pStmt

pApp :: Parser Expr
pApp =
  EApp <$> sp <*> pContExpr <*> parens (pExpr `sepBy` symbol ",")

pContExpr :: Parser Expr
pContExpr = choice
  [parens pExpr, ELit <$> sp <*> pValue, EVar <$> sp <*> pIdent]

pExprTerm :: Parser Expr
pExprTerm = choice [try pApp, pContExpr, try pFnDecl]

pExpr :: Parser Expr
pExpr = use opTable >>= makeExprParser pExprTerm

pContType :: Parser Type
pContType = choice [parens pType, Tyvar <$> sp <*> pIdent]

pFnType :: Parser Type
pFnType = do
  p <- sp
  reserved "fn"
  retType  <- pContType
  argTypes <- parens $ pType `sepBy` symbol ","
  return $ FnType p retType argTypes

pType :: Parser Type
pType = choice [try pContType, pFnType]

pStmt :: Parser Stmt
pStmt = choice
  [ pLet
  , try pAssign
  , ExprStmt <$> sp <*> (pExpr `sepBy1` symbol ",")
  , pIf
  , pIfElse
  , pFor
  , pWhile
  , pBreak
  , pContinue
  , pReturn
  ]

pLet :: Parser Stmt
pLet = withPos $ do
  s <- sp
  reserved "let"
  letItems <- pLetItem `sepBy1` symbol ","
  return $ Let s letItems

pLetItem :: Parser (Ident, Maybe Type, Expr)
pLetItem = do
  x     <- pIdent
  xType <- option Nothing (reserved ":" >> (Just <$> (try pType)))
  reserved "="
  val <- pExpr
  return (x, xType, val)

pAssign :: Parser Stmt
pAssign = withPos $ Assign <$> sp <*> pAssignItem `sepBy1` symbol ","

pAssignItem :: Parser (Ident, Expr)
pAssignItem = do
  x <- pIdent
  reserved "="
  val <- pExpr
  return (x, val)

pIfHead :: Parser (SourcePos, Expr)
pIfHead = do
  s <- sp
  reserved "if"
  cond <- parens pExpr
  return (s, cond)

pIf :: Parser Stmt
pIf = do
  ((s, cond), body) <- withBlock (,) pIfHead pStmt
  return $ If s cond body

pElseHead :: Parser ()
pElseHead = void $ reserved "else"

pIfElse :: Parser Stmt
pIfElse = do
  ((s, cond), then') <- withBlock (,) pIfHead pStmt
  else'              <- withBlock (flip const) pElseHead pStmt
  return $ IfElse s cond then' else'

pForHead :: Parser ([Stmt] -> Stmt)
pForHead = do
  s <- sp
  reserved "for"
  (x, seq) <- parens $ (,) <$> pIdent <* reserved "in" <*> pExpr
  return $ For s x seq

pFor :: Parser Stmt
pFor = withBlock ($) pForHead pStmt

pWhileHead :: Parser ([Stmt] -> Stmt)
pWhileHead = do
  s <- sp
  reserved "while"
  cond <- parens pExpr
  return $ While s cond

pWhile :: Parser Stmt
pWhile = withBlock ($) pWhileHead pStmt

pBreak :: Parser Stmt
pBreak = withPos $ do
  s <- sp
  reserved "break"
  return (Break s)

pContinue :: Parser Stmt
pContinue = withPos $ do
  s <- sp
  reserved "continue"
  return (Continue s)

pReturn :: Parser Stmt
pReturn = withPos $ do
  s <- sp
  reserved "return"
  retVal <- option Nothing (Just <$> (try pExpr))
  return $ Return s retVal
