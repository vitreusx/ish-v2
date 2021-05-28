{-# LANGUAGE DeriveFunctor, StandaloneDeriving #-}

module Syntax where

import           Value
import           Text.Megaparsec.Pos            ( SourcePos )

-- TopLevel
data TopLevel' a = TopLevel a [Stmt' a]
  deriving Functor

deriving instance Show a => Show (TopLevel' a)
type TopLevel = TopLevel' SourcePos

-- Ident
data Ident' a = Ident a String
  deriving Functor

deriving instance Show a => Show (Ident' a)
type Ident = Ident' SourcePos

-- Expr
data Expr' a =
  ELit a Value
  | EVar a (Ident' a)
  | EApp a (Expr' a) [Expr' a]
  | EPrefix a (Ident' a) (Expr' a)
  | EInfix a (Ident' a) (Expr' a) (Expr' a)
  | EPostfix a (Ident' a) (Expr' a)
  | EFnDecl a (Maybe (Type' a)) [(Ident' a, Type' a)] [Stmt' a]
  deriving Functor

deriving instance Show a => Show (Expr' a)
type Expr = Expr' SourcePos

-- Type
data Type' a =
  Tyvar a (Ident' a)
  | FnType a (Type' a) [Type' a]
  deriving Functor

deriving instance Show a => Show (Type' a)
type Type = Type' SourcePos

-- Stmt
data Stmt' a =
  Let a [(Ident' a, Maybe (Type' a), Expr' a)]
  | Assign a [(Ident' a, Expr' a)]
  | ExprStmt a [Expr' a]
  | If a (Expr' a) [Stmt' a]
  | IfElse a (Expr' a) [Stmt' a] [Stmt' a]
  | For a (Ident' a) (Expr' a) [Stmt' a]
  | While a (Expr' a) [Stmt' a]
  | Break a
  | Continue a
  | Return a (Maybe (Expr' a))
  deriving Functor

deriving instance Show a => Show (Stmt' a)
type Stmt = Stmt' SourcePos
