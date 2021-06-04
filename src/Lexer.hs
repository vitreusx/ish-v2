{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}

module Lexer where

import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as Lex
import           Control.Monad                  ( void )
import           Text.Read.Lex

lineComment :: MonadParsec e String m => m ()
lineComment = Lex.skipLineComment "--"

blockComment :: MonadParsec e String m => m ()
blockComment = Lex.skipBlockComment "/*" "*/"

sc :: MonadParsec e String m => m ()
sc = Lex.space space1 lineComment blockComment

lexeme :: MonadParsec e String m => m a -> m a
lexeme = Lex.lexeme sc

symbol :: MonadParsec e String m => String -> m String
symbol = Lex.symbol sc

ident :: MonadParsec e String m => m String
ident =
  lexeme ((:) <$> letterChar <*> many alphaNumChar <?> "identifier")

charLit :: MonadParsec e String m => m Char
charLit = between (char '\'') (char '\'') Lex.charLiteral

stringLit :: MonadParsec e String m => m String
stringLit = char '\"' *> manyTill Lex.charLiteral (char '\"')

intLit :: MonadParsec e String m => m Integer
intLit = lexeme Lex.decimal

realLit :: MonadParsec e String m => m Double
realLit = lexeme Lex.float

parens :: MonadParsec e String m => m a -> m a
parens = between (symbol "(") (symbol ")")

squareBrackets :: MonadParsec e String m => m a -> m a
squareBrackets = between (symbol "[") (symbol "]")

opIdent :: MonadParsec e String m => m String
opIdent = lexeme (some $ satisfy isSymbolChar)
