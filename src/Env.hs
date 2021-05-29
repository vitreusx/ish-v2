{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts, UndecidableInstances, RankNTypes #-}

module Env where

import           Control.Lens
import           Ontology
import           Data.Map.Strict                ( Map
                                                , (!)
                                                )
import qualified Data.Map.Strict               as Map
import           Control.Monad.State
import           Data.Maybe
import           Control.Monad.Identity
import           Syntax
import           Data.List
import           Control.Monad.Except

type ScopeIdx = Integer
type NameIdx = Integer
type VarIdx = (ScopeIdx, NameIdx)

data TypeIdx =
  TyvarIdx NameIdx
  | FnIdx NameIdx TypeIdx [TypeIdx]
  deriving (Eq, Ord, Show)

data TypePat =
  Exact TypeIdx | Any | InferFn [TypeIdx]

data VarEntry = VarEntry
  { _varID     :: VarIdx
  , _varDecl   :: Ident
  , _varTypeID :: TypeIdx
  }
  deriving (Eq, Ord, Show)

data TypeEntry = TypeEntry
  { _typeID   :: TypeIdx
  , _typeDecl :: Typename
  , _typeval  :: Type
  }
  deriving (Eq, Ord, Show)

data Scope = Scope
  { _scopeIdx   :: ScopeIdx
  , _vars       :: [VarEntry]
  , _freeVarIdx :: NameIdx
  , _withinLoop :: Bool
  , _returnType :: Maybe Type
  }
  deriving (Eq, Ord, Show)

data Env = Env
  { _scopes       :: Map ScopeIdx Scope
  , _freeScopeIdx :: ScopeIdx
  , _scopeStack   :: [ScopeIdx]
  , _types        :: [TypeEntry]
  , _freeTypeIdx  :: NameIdx
  }
  deriving (Eq, Ord, Show)

data EnvError =
  VarNotFound Ident
  | VarIdxNotFound VarIdx
  | AmbiguousType Ident
  | TypeNotFound Typename
  | TypeIdxNotFound TypeIdx

class MonadError EnvError m => MonadEnv m where
  enter, curScope :: Functor f => m (LensLike' f Env Scope)

  declareT :: Typename -> Type -> m TypeEntry
  nameLookupT :: Typename -> m TypeEntry
  idLookupT :: TypeIdx -> m TypeEntry

  declareV :: Ident -> Typename -> m VarEntry
  nameLookupV :: (Ident, TypePat) -> m VarEntry
  idLookupV :: VarIdx -> m VarEntry

  leave :: m ()

$(makeLenses ''VarEntry)
$(makeLenses ''TypeEntry)
$(makeLenses ''Scope)
$(makeLenses ''Env)

matches :: TypePat -> TypeIdx -> Bool
matches pat t = case (pat, t) of
  (Any          , _               ) -> True
  (Exact   t1   , t2              ) -> t1 == t2
  (InferFn targs, FnIdx _ _ targs') -> targs == targs'

newScope :: ScopeIdx -> Scope
newScope idx = Scope { _vars       = []
                     , _freeVarIdx = 0
                     , _withinLoop = False
                     , _returnType = Nothing
                     , _scopeIdx   = idx
                     }

subscope :: ScopeIdx -> Scope -> Scope
subscope idx s = (newScope idx) { _withinLoop = s ^. withinLoop
                                , _returnType = s ^. returnType
                                }

newEnv :: Env
newEnv = Env { _scopes       = Map.empty
             , _freeScopeIdx = 0
             , _scopeStack   = []
             , _types        = []
             , _freeTypeIdx  = 0
             }

type IsoLike' p f s a = p a (f a) -> p s (f s)

assumeExists
  :: forall f p a
   . (Profunctor p, Functor f)
  => IsoLike' p f (Maybe a) a
assumeExists = iso fromJust Just

strip :: Functor f => f a -> f ()
strip = fmap (const ())

instance (MonadError EnvError m, MonadState Env m) => MonadEnv m where
  enter = do
    newScopeIdx' <- use freeScopeIdx
    scopeStack'  <- use scopeStack
    scope'       <- if scopeStack' == []
      then return (newScope newScopeIdx')
      else do
        scopeView <- curScope
        uses scopeView (subscope newScopeIdx')

    scopes . (at newScopeIdx') .= Just scope'
    scopeStack %= (:) newScopeIdx'
    freeScopeIdx %= (+ 1)

    return $ scopes . (at newScopeIdx') . assumeExists

  curScope = do
    curScopeIdx <- uses scopeStack head
    return $ scopes . (at curScopeIdx) . assumeExists

  declareT t tval = do
    typeIdx <- case t of
      Tyvar _ _             -> TyvarIdx <$> use freeTypeIdx
      FnTypename _ tr targs -> do
        idx <- use freeTypeIdx
        let typeidOf t' = do
              ent <- nameLookupT t'
              return (ent ^. typeID)
        trIdx    <- typeidOf tr
        targsIdx <- mapM typeidOf targs
        return $ FnIdx idx trIdx targsIdx

    let entry = TypeEntry { _typeID   = typeIdx
                          , _typeDecl = t
                          , _typeval  = tval
                          }

    freeTypeIdx %= (+ 1)
    types %= (:) entry
    return entry

  nameLookupT t = do
    case t of
      Tyvar _ _        -> tyvarLookupT
      FnTypename _ _ _ -> fnLookupT
   where
    tyvarLookupT = do
      mentry <- tyvarLookupT'
      case mentry of
        Just entry -> return entry
        Nothing    -> throwError $ TypeNotFound t
    tyvarLookupT' = do
      let filt tent = (strip (tent ^. typeDecl) == strip t)
      types' <- use types
      return $ find filt types'
    fnLookupT = do
      mentry <- tyvarLookupT'
      case mentry of
        Just entry -> return entry
        Nothing    -> genFuncT
    genFuncT = do
      let FnTypename _ tr targs = t
      let typevalOf t' = do
            ent <- nameLookupT t'
            return (ent ^. typeval)
      trv    <- typevalOf tr
      targsv <- mapM typevalOf targs
      let tval = FnT trv targsv
      declareT (fmap (const $ -1) t) tval

  idLookupT typeIdx = do
    let matches ent = (ent ^. typeID == typeIdx)
    ment <- uses types (find matches)
    case ment of
      Just ent -> return ent
      Nothing  -> throwError $ TypeIdxNotFound typeIdx

  declareV name t = do
    scopeUse  <- curScope
    scopeMod  <- curScope

    t'        <- nameLookupT t
    varIdx    <- use (scopeUse . freeVarIdx)
    scopeIdx' <- use (scopeUse . scopeIdx)
    let typeIdx = t' ^. typeID
        entry   = VarEntry { _varID     = (scopeIdx', varIdx)
                           , _varDecl   = name
                           , _varTypeID = typeIdx
                           }

    scopeMod . freeVarIdx %= (+ 1)
    scopeMod . vars %= (:) entry
    return entry

  nameLookupV (name, pat) = do
    scopeStack' <- use scopeStack
    matching    <- mapM extract scopeStack'
    let flat = concat matching
    case length flat of
      0 -> throwError $ VarNotFound name
      1 -> return (head flat)
      _ -> throwError $ AmbiguousType name
   where
    extract scopeIdx = do
      vars' <- use (scopes . (at scopeIdx) . assumeExists . vars)
      let filt ent = do
            t' <- typeofV ent
            return (matches pat t')
      filterM filt vars'
    typeofV ent = do
      let varTypeIdx = ent ^. varTypeID
      tent <- idLookupT varTypeIdx
      return (tent ^. typeID)

  idLookupV (scopeIdx, nameIdx) = do
    vars' <- use $ scopes . (at scopeIdx) . assumeExists . vars
    let ment =
          find (\ent -> ent ^. varID == (scopeIdx, nameIdx)) vars'
    case ment of
      Just ent -> return ent
      Nothing  -> throwError $ VarIdxNotFound (scopeIdx, nameIdx)

  leave = do
    scopeStack %= tail
