{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}

module Lexer where

import           ParserDef
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as Lex
import           Data.Text                      ( Text )
import           Control.Monad                  ( void )

lineComment :: Parser ()
lineComment = Lex.skipLineComment "//"

blockComment :: Parser ()
blockComment = Lex.skipBlockComment "/*" "*/"

scn :: Parser ()
scn = Lex.space space1 lineComment blockComment

sc :: Parser ()
sc = Lex.space (void $ some $ char ' ') lineComment blockComment

lexeme :: Parser a -> Parser a
lexeme = Lex.lexeme sc

symbol :: String -> Parser String
symbol = Lex.symbol sc

ident :: Parser String
ident =
  lexeme ((:) <$> letterChar <*> many alphaNumChar <?> "identifier")

charLit :: Parser Char
charLit = between (char '\'') (char '\'') Lex.charLiteral

stringLit :: Parser String
stringLit = char '\"' *> manyTill Lex.charLiteral (char '\"')

intLit :: Parser Integer
intLit = lexeme Lex.decimal

realLit :: Parser Double
realLit = lexeme Lex.float

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")
