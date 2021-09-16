{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module VM where

import           Syntax
import qualified Parser                        as Par
import           Control.Lens
import           Control.Monad.Cont
import           Control.Monad.State
import           Control.Monad.Except
import           Control.Monad.Extra
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map
import           Data.Maybe
import           Text.Megaparsec
import           Data.List

data FuncDef =
  Defined Par.Expr
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
  | Kind | Phantom Value | Ref Ptr
  | Jump (Eval ()) | ReturnJ Bool Value (Value -> Eval ()) | JumpT
  deriving (Eq, Show)

data TypePat =
  ExactType Value
  | FnPat [Value]
  | AnyType
  deriving (Eq, Show)

data SymbolPat =
  ExactSym Symbol
  | FromName String
  deriving (Eq, Show)

type Ptr = Int

data Symbol =
  Var Par.Ident
  | Return | Break | Continue | Recur
  deriving (Eq, Show)

data Env = Env
  { _symbols   :: [(Symbol, Ptr)]
  , _typecheck :: Bool
  }
  deriving (Eq, Show)

data VM = VM
  { _curEnv  :: Env
  , _memory  :: Map Ptr Value
  , _freePtr :: Ptr
  }
  deriving (Eq, Show)

data VMError =
  NotFound SymbolPat TypePat
  | InvalidType Par.Expr TypePat
  | InvalidReturn Par.Stmt Value
  | PatternError String
  | DivisionByZero
  deriving (Eq, Show)

type Eval = ContT () (StateT VM IO)

instance MonadError VMError Eval where
  throwError e = do
    liftIO (fail $ show e)

  catchError try _ = try

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

$(makeLenses ''FnValue)
$(makeLenses ''Env)
$(makeLenses ''VM)

mkEnv :: Env
mkEnv = Env { _symbols = [], _typecheck = False }

mkVM :: VM
mkVM = VM { _curEnv = mkEnv, _memory = Map.empty, _freePtr = 0 }

typeof :: Value -> Value
typeof (IntV    _)     = IntT
typeof (StringV _)     = StringT
typeof (BoolV   _)     = BoolT
typeof VoidV           = VoidT
typeof (FnV fnv)       = fnv ^. fnType
typeof IntT            = Kind
typeof StringT         = Kind
typeof VoidT           = Kind
typeof (FnT _ _)       = Kind
typeof BoolT           = Kind
typeof Kind            = Kind
typeof (Phantom t    ) = t
typeof (Jump    _    ) = JumpT
typeof (ReturnJ _ _ _) = JumpT
typeof JumpT           = Kind

typeMatches :: Value -> TypePat -> Bool
typeMatches (FnT _ ta1) (FnPat     ta2) = ta1 == ta2
typeMatches t1          (ExactType t2 ) = t1 == t2
typeMatches t           AnyType         = typeof t == Kind
typeMatches _           _               = False

symMatches :: Symbol -> SymbolPat -> Bool
symMatches s1                 (ExactSym s2) = s1 == s2
symMatches (Var (Ident _ x1)) (FromName x2) = x1 == x2
symMatches _                  _             = False

isPhantom :: Value -> Bool
isPhantom (Phantom _) = True
isPhantom _           = False

assumeEx
  :: (Profunctor p, Functor f)
  => p a (f a)
  -> p (Maybe a) (f (Maybe a))
assumeEx = iso fromJust Just

scoped :: Eval a -> Eval a
scoped x = do
  env <- use curEnv
  v   <- x
  curEnv .= env
  return v

lookup
  :: Functor f => SymbolPat -> TypePat -> Eval (LensLike' f VM Value)
lookup xpat tpat = do
  symbols' <- use $ curEnv . symbols
  mv       <- findM matches' symbols'
  case mv of
    Nothing       -> throwError (NotFound xpat tpat)
    Just (_, ptr) -> return $ memory . (at ptr) . assumeEx
 where
  matches' (x', ptr') = do
    if x' `symMatches` xpat
      then do
        v <- use $ memory . (at ptr') . assumeEx
        return $ (typeof v) `typeMatches` tpat
      else return False

declare :: Symbol -> Value -> Eval ()
declare x v = do
  xPtr <- malloc
  curEnv . symbols %= (:) (x, xPtr)
  memory . (at xPtr) .= Just v

malloc :: Eval Ptr
malloc = do
  xPtr <- use freePtr
  freePtr += 1
  return xPtr
