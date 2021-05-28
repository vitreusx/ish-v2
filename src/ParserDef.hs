{-# LANGUAGE TemplateHaskell #-}

module ParserDef where

import           Text.Megaparsec         hiding ( State
                                                , ParseError
                                                )
import           Control.Monad.Combinators.Expr
import           Control.Monad.State.Strict
import           Data.Map.Strict
import           Data.Void                      ( Void )
import           Syntax
import           Control.Lens

type IndentInfo = (Int, Int)

data ParserState = ParserState
  { _opMap   :: [(String, Integer, Operator Parser Expr)]
  , _opTable :: [[Operator Parser Expr]]
  , _curRef  :: !IndentInfo
  }

data ParseError = IncorrectLine Int Int
  deriving (Eq, Ord, Show)

instance ShowErrorComponent ParseError where
  showErrorComponent x = case x of
    IncorrectLine ref actual ->
      "incorrect line (got "
        <> show actual
        <> ", should be "
        <> show ref
        <> ")"

type Parser = ParsecT ParseError String (State ParserState)

$(makeLenses ''ParserState)

parserState0 :: ParserState
parserState0 =
  ParserState { _opMap = [], _opTable = [], _curRef = (1, 1) }
