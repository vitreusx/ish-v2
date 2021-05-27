-- |

module Value where

data Value =
  IntV Integer
  | RealV Double
  | StringV String
  | BoolV Bool
  | VoidV
  deriving Show
