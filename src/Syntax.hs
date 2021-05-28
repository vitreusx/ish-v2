{-# LANGUAGE DeriveFunctor, StandaloneDeriving #-}

module Syntax where

import           Ontology
import           Text.Megaparsec.Pos            ( SourcePos )

type Anchor = Int

-- TopLevel
data TopLevel' a = TopLevel a [Stmt' a]
  deriving Functor

deriving instance Eq a => Eq (TopLevel' a)
deriving instance Ord a => Ord (TopLevel' a)
deriving instance Show a => Show (TopLevel' a)
type TopLevel = TopLevel' Anchor

-- Ident
data Ident' a = Ident a String
  deriving Functor

deriving instance Eq a => Eq (Ident' a)
deriving instance Ord a => Ord (Ident' a)
deriving instance Show a => Show (Ident' a)
type Ident = Ident' Anchor

-- Expr
data Expr' a =
  ELit a Value
  | EVar a (Ident' a)
  | EApp a (Expr' a) [Expr' a]
  | EPrefix a (Ident' a) (Expr' a)
  | EInfix a (Ident' a) (Expr' a) (Expr' a)
  | EPostfix a (Ident' a) (Expr' a)
  | EFnDecl a (Maybe (Typename' a)) [(Ident' a, Typename' a)] [Stmt' a]
  deriving Functor

deriving instance Eq a => Eq (Expr' a)
deriving instance Ord a => Ord (Expr' a)
deriving instance Show a => Show (Expr' a)
type Expr = Expr' Anchor

-- Type
data Typename' a =
  Tyvar a (Ident' a)
  | FnTypename a (Typename' a) [Typename' a]
  deriving Functor

deriving instance Eq a => Eq (Typename' a)
deriving instance Ord a => Ord (Typename' a)
deriving instance Show a => Show (Typename' a)
type Typename = Typename' Anchor

-- Stmt
data Stmt' a =
  Let a [(Ident' a, Maybe (Typename' a), Expr' a)]
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

deriving instance Eq a => Eq (Stmt' a)
deriving instance Ord a => Ord (Stmt' a)
deriving instance Show a => Show (Stmt' a)
type Stmt = Stmt' Anchor
