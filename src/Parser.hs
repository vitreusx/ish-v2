{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Parser where

import           Text.Megaparsec         hiding ( State
                                                , ParseError
                                                )
import           Text.Megaparsec.Pos           as Pos
import           Control.Monad.Combinators.Expr
import           Control.Monad.State
import           Data.Map.Strict               as Map
import           Syntax
import           Control.Lens
import qualified Data.Set                      as Set

newtype SRef = SRef Pos.SourcePos
  deriving (Eq, Ord)

instance Show SRef where
  show (SRef r) = sourcePosPretty r

type TopLevel = TopLevel' SRef
type Ident = Ident' SRef
type Expr = Expr' SRef
type Stmt = Stmt' SRef
type LetItem = LetItem' SRef
type AssignItem = AssignItem' SRef

ref :: Parser SRef
ref = SRef <$> getSourcePos

type IndentInfo = (Int, Int)

data ParserEnv = ParserEnv
  { _opMap     :: [(Integer, Operator Parser Expr)]
  , _opTable   :: [[Operator Parser Expr]]
  , _refIndent :: IndentInfo
  }

data ParseError =
  IncorrectLine Int Int
  | IncorrectIndent Ordering Int Int
  deriving (Eq, Ord, Show)

instance ShowErrorComponent ParseError where
  showErrorComponent x = show x

type Parser = ParsecT ParseError String (StateT ParserEnv IO)

$(makeLenses ''ParserEnv)

mkParserEnv :: ParserEnv
mkParserEnv =
  ParserEnv { _opMap = [], _opTable = [], _refIndent = (1, 1) }

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

throwParseError :: ParseError -> Parser a
throwParseError e = fancyFailure (Set.singleton (ErrorCustom e))
