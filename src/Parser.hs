{-# LANGUAGE TupleSections #-}

module Parser where

import           Lexer
import           ParserDef
import           Syntax
import           Value
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as Lex
import           Control.Monad.Combinators.Expr ( makeExprParser )
import           Control.Lens
import           Control.Monad                  ( void )
import           Control.Monad.State.Strict

sp :: Parser SourcePos
sp = getSourcePos

-- pTopLevel :: Parser TopLevel
-- pTopLevel =
--   TopLevel
--     <$> sp
--     <*> (Lex.nonIndented scn (Lex.indentBlock scn p) <* eof)
--   where p = return $ Lex.IndentMany Nothing return pStmt
pTopLevel :: Parser TopLevel
pTopLevel = TopLevel <$> sp <*> (pStmt `sepBy1` symbol ";")

reservedWords :: [String]
reservedWords =
  [ "if"
  , "then"
  , "else"
  , "return"
  , "continue"
  , "break"
  , "fn"
  , "true"
  , "false"
  , "for"
  , "let"
  ]

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
  [ try (IntV <$> intLit)
  , RealV <$> realLit
  , StringV <$> stringLit
  , try (symbol "true" *> (return $ BoolV True))
  , try (symbol "false" *> (return $ BoolV False))
  ]

pScope' :: Parser a -> Parser b -> Parser (a, [b])
pScope' header items = Lex.indentBlock scn p
 where
  p = do
    header' <- header
    return $ Lex.IndentSome Nothing (return . (header', )) items

pScope :: Parser a -> Parser (a, [Stmt])
pScope header = pScope' header pStmt

pFnArg :: Parser (Ident, Type)
pFnArg = do
  x <- pIdent
  symbol ":"
  t <- pType
  return (x, t)

pFnDeclHead :: Parser ([Stmt] -> Expr)
pFnDeclHead = do
  p <- sp
  symbol "fn"
  retType <- option Nothing (Just <$> pType)
  args    <- parens $ pFnArg `sepBy` symbol ","
  return $ EFnDecl p retType args

pFnDecl :: Parser Expr
pFnDecl = do
  (head, body) <- pScope pFnDeclHead
  return (head body)

pExprTerm :: Parser Expr
pExprTerm = choice
  [ parens pExpr
  , ELit <$> sp <*> pValue
  , EVar <$> sp <*> pIdent
  , try pFnDecl
  ]

pExpr :: Parser Expr
pExpr = do
  parserState <- get
  let opTable' = parserState ^. opTable
  makeExprParser pExprTerm opTable'

pFnType :: Parser Type
pFnType = do
  p <- sp
  symbol "fn"
  retType  <- pType
  argTypes <- parens $ pType `sepBy` symbol ","
  return $ FnType p retType argTypes

pType :: Parser Type
pType = choice [Tyvar <$> sp <*> pIdent, pFnType]

pStmt :: Parser Stmt
pStmt = choice
  [ pLet
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
pLet = do
  s <- sp
  symbol "let"
  letItems <- pLetItem `sepBy1` symbol ","
  return $ Let s letItems

pLetItem :: Parser (Ident, Maybe Type, Expr)
pLetItem = do
  x     <- pIdent
  xType <- try $ option Nothing (symbol ":" >> (Just <$> pType))
  symbol ":="
  val <- pExpr
  return (x, xType, val)

pIfHead :: Parser (SourcePos, Expr)
pIfHead = do
  s <- sp
  symbol "if"
  cond <- parens pExpr
  return (s, cond)

pIf :: Parser Stmt
pIf = do
  ((s, cond), body) <- pScope pIfHead
  return $ If s cond body

pElseHead :: Parser ()
pElseHead = void $ symbol "else"

pIfElse :: Parser Stmt
pIfElse = do
  ((s, cond), then') <- pScope pIfHead
  (_        , else') <- pScope pElseHead
  return $ IfElse s cond then' else'

pForHead :: Parser ([Stmt] -> Stmt)
pForHead = do
  s <- sp
  symbol "for"
  (x, seq) <- parens $ (,) <$> pIdent <*> pExpr
  return $ For s x seq

pFor :: Parser Stmt
pFor = do
  (head, body) <- pScope pForHead
  return (head body)

pWhileHead :: Parser ([Stmt] -> Stmt)
pWhileHead = do
  s <- sp
  symbol "while"
  cond <- parens pExpr
  return $ While s cond

pWhile :: Parser Stmt
pWhile = do
  (head, body) <- pScope pWhileHead
  return (head body)

pBreak :: Parser Stmt
pBreak = do
  s <- sp
  symbol "break"
  return (Break s)

pContinue :: Parser Stmt
pContinue = do
  s <- sp
  symbol "continue"
  return (Continue s)

pReturn :: Parser Stmt
pReturn = do
  s <- sp
  symbol "return"
  retVal <- option Nothing (Just <$> pExpr)
  return $ Return s retVal
