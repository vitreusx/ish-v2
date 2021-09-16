{-# LANGUAGE FlexibleContexts #-}

module Lexer where

import           Parser
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as Lex
import           Control.Monad                  ( void )
import           Text.Read.Lex

lineComment :: Parser ()
lineComment = Lex.skipLineComment "~" >> scn

sc :: Parser ()
sc = Lex.space hspace1 lineComment empty

scn :: Parser ()
scn = Lex.space space1 lineComment empty

lexeme :: Parser a -> Parser a
lexeme = Lex.lexeme sc

symbol :: String -> Parser String
symbol = Lex.symbol sc

ident :: Parser String
ident = lexeme ((:) <$> letterChar <*> many alphaNumChar)

charLit :: Parser Char
charLit = lexeme $ between (char '\'') (char '\'') Lex.charLiteral

stringLit :: Parser String
stringLit = lexeme $ char '"' >> manyTill Lex.charLiteral (char '"')

intLit :: Parser Integer
intLit = lexeme Lex.decimal

realLit :: Parser Double
realLit = lexeme Lex.float

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

squareBrackets :: Parser a -> Parser a
squareBrackets = between (symbol "[") (symbol "]")

opIdent :: Parser String
opIdent = lexeme (some $ satisfy isSymbolChar)
