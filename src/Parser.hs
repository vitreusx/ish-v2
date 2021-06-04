{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Parser where

import           Text.Megaparsec         hiding ( State
                                                , ParseError
                                                )
import qualified Text.Megaparsec               as Mp
import           Control.Monad.Combinators.Expr
import           Control.Monad.State
import           Data.Map.Strict               as Map
import           Syntax
import           Control.Lens
import qualified Data.Set                      as Set

type Ref = Mp.State String ParseError

instance {-# OVERLAPS #-} Show Ref where
  show x = "[Ref]"

type TopLevel = TopLevel' Ref
type Ident = Ident' Ref
type Expr = Expr' Ref
type Stmt = Stmt' Ref
type LetItem = LetItem' Ref
type AssignItem = AssignItem' Ref

ref :: Parser Ref
ref = getParserState

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
