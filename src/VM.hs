{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module VM where

import           Syntax                         ( Ptr
                                                , Loc
                                                )
import qualified Syntax                        as Syn
import           Control.Lens
import           Control.Monad.Cont
import           Control.Monad.State
import           Control.Monad.Except
import           Control.Monad.Extra
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map
import           Data.Maybe

data FuncDef =
  Defined Syn.Expr
  | Intrinsic ([Value] -> Eval Value)
  deriving (Eq, Show)

data FnValue = FnValue
  { _fnType  :: Value
  , _fnDef   :: FuncDef
  , _closure :: Env
  }
  deriving (Eq, Show)

data Value =
  IntV Integer | StringV String | BoolV Bool | VoidV | FnV FnValue
  | IntT | StringT | VoidT | FnT Value [Value] | BoolT
  | Kind | AnyT | Any | Phantom Value
  | Jump (Eval ()) | ReturnJ (Value -> Eval ()) Value | JumpT
  deriving (Eq, Show)

data Symbol =
  Var Syn.Ident
  | Return | Break | Continue
  deriving (Eq, Show)

data Env = Env
  { _symbols :: [(Symbol, Ptr)]
  }
  deriving (Eq, Show)

data VM = VM
  { _curEnv  :: Env
  , _memory  :: Map Ptr Value
  , _freePtr :: Ptr
  }
  deriving (Eq, Show)

data VMError =
  NotFound
  | InvalidType
  | PatternError String
  deriving (Eq, Show)

type Eval = ExceptT VMError (ContT () (StateT VM IO))

instance {-# OVERLAPS #-} Eq (Eval ()) where
  (==) _ _ = False

instance {-# OVERLAPS #-} Show (Eval ()) where
  show x = "(Eval ())"

instance {-# OVERLAPS #-} Eq (Value -> Eval ()) where
  (==) _ _ = False

instance {-# OVERLAPS #-} Show (Value -> Eval ()) where
  show x = "(Value -> Eval ())"

instance {-# OVERLAPS #-} Eq ([Value] -> Eval Value) where
  (==) _ _ = False

instance {-# OVERLAPS #-} Show ([Value] -> Eval Value) where
  show x = "([Value] -> Eval Value)"

instance {-# OVERLAPS #-} MonadFail Eval where
  fail s = throwError (PatternError s)

$(makeLenses ''FnValue)
$(makeLenses ''Env)
$(makeLenses ''VM)

env0 :: Env
env0 = Env { _symbols = [] }

vm0 :: VM
vm0 = VM { _curEnv = env0, _memory = Map.empty, _freePtr = 0 }

assumeEx
  :: (Profunctor p, Functor f)
  => p a (f a)
  -> p (Maybe a) (f (Maybe a))
assumeEx = iso fromJust Just

checkType :: Value -> Value -> Eval Value
checkType t x =
  if (typeof x) `matches` t then return x else throwError InvalidType

typeof :: Value -> Value
typeof (IntV    _  ) = IntT
typeof (StringV _  ) = StringT
typeof (FnV     fnv) = fnv ^. fnType
typeof IntT          = Kind
typeof StringT       = Kind
typeof VoidT         = Kind
typeof (FnT _ _)     = Kind
typeof Kind          = Kind
typeof (Jump _     ) = JumpT
typeof (ReturnJ _ _) = JumpT
typeof JumpT         = Kind

matches :: Value -> Value -> Bool
v  `matches` Any  = True
t  `matches` AnyT = (typeof t) == Kind
t1 `matches` t2   = t1 == t2

nameMatches :: Symbol -> Symbol -> Bool
nameMatches (Var x1) (Var x2) = (Syn.nameof x1) == (Syn.nameof x2)
nameMatches x        y        = x == y

scoped :: Eval a -> Eval a
scoped x = do
  env <- use curEnv
  v   <- x
  curEnv .= env
  return v

lookup :: Symbol -> Value -> Eval (ALens' VM Value)
lookup x t = do
  symbols' <- use $ curEnv . symbols
  mv       <- findM matches' symbols'
  case mv of
    Nothing       -> throwError NotFound
    Just (_, ptr) -> return $ memory . (at ptr) . assumeEx
 where
  matches' (x', ptr') = do
    if nameMatches x' x
      then do
        v <- use $ memory . (at ptr') . assumeEx
        return $ (typeof v) `matches` t
      else return False

declare :: Symbol -> Value -> Eval ()
declare x v = do
  xPtr <- use freePtr
  curEnv . symbols %= (:) (x, xPtr)
  memory . (at xPtr) .= Just v
  freePtr += 1
