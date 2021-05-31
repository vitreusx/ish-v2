{-# LANGUAGE TemplateHaskell #-}

module ParserDef where

import           Text.Megaparsec         hiding ( State
                                                , ParseError
                                                )
import           Control.Monad.Combinators.Expr
import           Control.Monad.State.Strict
import           Data.Map.Strict               as Map
import           Data.Void                      ( Void )
import           Syntax
import           Control.Lens

type IndentInfo = (Int, Int)

data ParserState = ParserState
  { _opMap   :: [(Integer, Operator Parser Expr)]
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

addOperator :: (Integer, Operator Parser Expr) -> Parser ()
addOperator op = do
  opMap %= (:) op
  opTable <~ reconstructOpTable

reconstructOpTable :: Parser [[Operator Parser Expr]]
reconstructOpTable = do
  opMap' <- use opMap
  let grouped = Map.fromListWith
        (++)
        [ (prec, [op]) | (prec, op) <- opMap' ]
      unraveled = Map.elems grouped
  return unraveled

setPos :: (FilePath, Pos, Pos) -> Parser ()
setPos (file, line, col) = do
  st <- getParserState
  let curPosState = statePosState st
      thPos       = SourcePos { sourceName   = file
                              , sourceLine   = line
                              , sourceColumn = col
                              }
      finPosState = curPosState { pstateSourcePos = thPos }
      finState    = st { statePosState = curPosState }
  setParserState st
