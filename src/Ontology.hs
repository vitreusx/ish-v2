-- |

module Ontology where

data Value =
  IntV Integer
  | RealV Double
  | StringV String
  | BoolV Bool
  deriving (Eq, Ord, Show)

data Type =
  IntT | RealT | StringT | BoolT | VoidT | FnT Type [Type]
  deriving (Eq, Ord, Show)

typeof :: Value -> Type
typeof (IntV    _) = IntT
typeof (RealV   _) = RealT
typeof (StringV _) = StringT
typeof (BoolV   _) = BoolT
