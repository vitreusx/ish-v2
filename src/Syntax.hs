{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Syntax where

import           Text.Megaparsec.Pos            ( SourcePos )
import           Data.Generics

type Attachment = Int
type Ptr = Integer
type Loc = Integer

-- TopLevel
data TopLevel' a = TopLevel a [Stmt' a]
  deriving (Functor, Typeable, Data)
type TopLevel = TopLevel' Attachment

deriving instance Eq a => Eq (TopLevel' a)
deriving instance Ord a => Ord (TopLevel' a)
deriving instance Show a => Show (TopLevel' a)

-- Ident
data Ident' a = Ident a String | IntrinName String
  deriving (Functor, Typeable, Data)
type Ident = Ident' Attachment

deriving instance Eq a => Eq (Ident' a)
deriving instance Ord a => Ord (Ident' a)
deriving instance Show a => Show (Ident' a)

-- Expr
data Expr' a =
  ELitInt a Integer
  | ELitStr a String
  | EKind a
  | EVar (Ident' a)
  | EApp a (Expr' a) [Expr' a]
  | EPrefix a (Ident' a) (Expr' a)
  | EInfix a (Ident' a) (Expr' a) (Expr' a)
  | EPostfix a (Ident' a) (Expr' a)
  | EFnDecl a (Expr' a) [(Ident' a, Expr' a)] [Stmt' a]
  | ELam a (Expr' a) [(Ident' a, Expr' a)] (Expr' a)
  | EFnType a (Expr' a) [Expr' a]
  deriving (Functor, Typeable, Data)
type Expr = Expr' Attachment

deriving instance Eq a => Eq (Expr' a)
deriving instance Ord a => Ord (Expr' a)
deriving instance Show a => Show (Expr' a)

-- Stmt
data Stmt' a =
  Let a [LetItem' a]
  | Assign a [AssignItem' a]
  | ExprStmt [Expr' a]
  | If a (Expr' a) [Stmt' a]
  | IfElse a (Expr' a) [Stmt' a] [Stmt' a]
  | While a (Expr' a) [Stmt' a]
  | Break a
  | Continue a
  | Return a (Maybe (Expr' a))
  | Future a (Ident' a) (Expr' a)
  deriving (Functor, Typeable, Data)
type Stmt = Stmt' Attachment

deriving instance Eq a => Eq (Stmt' a)
deriving instance Ord a => Ord (Stmt' a)
deriving instance Show a => Show (Stmt' a)

type LetItem' a = (Ident' a, Maybe (Expr' a), Expr' a)
type LetItem = LetItem' Attachment

type AssignItem' a = (Ident' a, Expr' a)
type AssignItem = AssignItem' Attachment

instance Semigroup (TopLevel' a) where
  (TopLevel p1 x1) <> (TopLevel p2 x2) = TopLevel p1 (x1 <> x2)

nameof :: Ident' a -> String
nameof (Ident p x   ) = x
nameof (IntrinName x) = x
