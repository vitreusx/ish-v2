{-# LANGUAGE TemplateHaskell #-}

module ParserDef where

import           Text.Megaparsec         hiding ( State )
import           Control.Lens
import           Control.Monad.Combinators.Expr
import           Control.Monad.State.Strict
import           Data.Map.Strict
import           Data.Void                      ( Void )
import           Syntax

data ParserState = ParserState
  { _opMap   :: [(String, Integer, Operator Parser Expr)]
  , _opTable :: [[Operator Parser Expr]]
  }

type Parser = ParsecT Void String (State ParserState)

$(makeLenses ''ParserState)

parserState0 :: ParserState
parserState0 = ParserState { _opMap = [], _opTable = [] }
