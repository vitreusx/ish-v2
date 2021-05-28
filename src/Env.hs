{-# LANGUAGE TemplateHaskell, FlexibleInstances, FlexibleContexts, UndecidableInstances, RankNTypes #-}

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

type ScopeIdx = Integer
type NameIdx = Integer
type NameLoc = (ScopeIdx, NameIdx)

data VarEntry = VarEntry
  { _varID       :: NameLoc
  , _varName     :: Ident' ()
  , _varDecl     :: Ident
  , _varTypeID   :: NameLoc
  , _varTypename :: Typename' ()
  }
  deriving (Eq, Ord, Show)

data TypeEntry = TypeEntry
  { _typeID   :: NameLoc
  , _typename :: Typename' ()
  , _typeDecl :: Typename
  , _typeval  :: Type
  }
  deriving (Eq, Ord, Show)

data Scope = Scope
  { _scopeIdx      :: ScopeIdx
  , _vars          :: Map (Ident' ()) (Map NameLoc VarEntry)
  , _freeVarIdx    :: NameIdx
  , _types         :: Map (Typename' ()) TypeEntry
  , _typeIdxLookup :: Map NameIdx (Typename' ())
  , _freeTypeIdx   :: NameIdx
  , _withinLoop    :: Bool
  , _returnType    :: Maybe Type
  }
  deriving (Eq, Ord, Show)

data Env = Env
  { _scopes       :: Map ScopeIdx Scope
  , _freeScopeIdx :: ScopeIdx
  , _scopeStack   :: [ScopeIdx]
  }
  deriving (Eq, Ord, Show)

$(makeLenses ''VarEntry)
$(makeLenses ''TypeEntry)
$(makeLenses ''Scope)
$(makeLenses ''Env)

data TypePat =
  Exact (Typename' ()) | InferReturn [Typename' ()]

class MonadEnv m where
  enter, curScope :: Functor f => m (LensLike' f Env Scope)

  declareT :: Typename -> Type -> m TypeEntry
  nameLookupT :: Typename' () -> m TypeEntry

  declareV :: Ident -> Typename' () -> m VarEntry
  nameLookupV :: (Ident' (), TypePat) -> m VarEntry

  leave :: m ()

newScope :: ScopeIdx -> Scope
newScope idx = Scope { _vars          = Map.empty
                     , _freeVarIdx    = 0
                     , _types         = Map.empty
                     , _freeTypeIdx   = 0
                     , _withinLoop    = False
                     , _returnType    = Nothing
                     , _scopeIdx      = idx
                     , _typeIdxLookup = Map.empty
                     }

subscope :: ScopeIdx -> Scope -> Scope
subscope idx s = (newScope idx) { _withinLoop = s ^. withinLoop
                                , _returnType = s ^. returnType
                                }

newEnv :: Env
newEnv =
  let zeroEnv = Env { _scopes       = Map.empty
                    , _freeScopeIdx = 0
                    , _scopeStack   = []
                    }
      enter' = enter :: State Env (LensLike' Identity Env Scope)
  in  execState enter' zeroEnv

type IsoLike' p f s a = p a (f a) -> p s (f s)

assumeExists
  :: forall f p a
   . (Profunctor p, Functor f)
  => IsoLike' p f (Maybe a) a
assumeExists = iso fromJust Just

instance MonadState Env m => MonadEnv m where
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
    scopeView <- curScope
    scopeUse  <- curScope

    typeIdx   <- use (scopeView . freeTypeIdx)
    scopeIdx' <- use (scopeView . scopeIdx)
    let stripped = fmap (const ()) t
        entryVal = TypeEntry { _typeID   = (scopeIdx', typeIdx)
                             , _typename = stripped
                             , _typeDecl = t
                             , _typeval  = tval
                             }
        entry = scopeUse . types . (at stripped)

    scopeUse . freeTypeIdx %= (+ 1)
    entry .= Just entryVal
    return entryVal

  nameLookupT t = do
    case t of
      Tyvar _ _        -> tyvarLookupT
      FnTypename _ _ _ -> fnLookupT
   where
    tyvarLookupT = do
      scopeStack' <- use scopeStack
      found       <- mapM scopeLookupT scopeStack'
      let Just closest = msum found
      return closest
    scopeLookupT scopeIdx = do
      use $ scopes . (at scopeIdx) . assumeExists . types . (at t)
    fnLookupT = do
      let FnTypename _ tr targs = t
      tr'    <- nameLookupT tr
      targs' <- mapM nameLookupT targs

      let globalIdx   = 0 :: ScopeIdx
          globalScope = scopes . (at globalIdx) . assumeExists
      globalScope' <- use $ scopes . (at globalIdx) . assumeExists
      case globalScope' ^. types . (at t) of
        Just v' -> return v'
        Nothing -> do
          let
            typeIdx = globalScope' ^. freeTypeIdx
            entry   = TypeEntry
              { _typeID   = (globalIdx, typeIdx)
              , _typename = t
              , _typeDecl = fmap (const 0) t
              , _typeval  = FnT (tr' ^. typeval)
                                [ ta' ^. typeval | ta' <- targs' ]
              }
          globalScope . freeTypeIdx %= (+ 1)
          globalScope . types . (at t) .= Just entry
          return entry

  declareV name t = do
    t'        <- nameLookupT t
    scopeView <- curScope
    scopeUse  <- curScope

    varIdx    <- use (scopeView . freeVarIdx)
    scopeIdx' <- use (scopeView . scopeIdx)
    let typeIdx   = t' ^. typeID
        stripped  = fmap (const ()) name
        overloads = scopeUse . vars . (at stripped) . (non Map.empty)
        entryVal  = VarEntry { _varID       = (scopeIdx', varIdx)
                             , _varName     = stripped
                             , _varDecl     = name
                             , _varTypeID   = typeIdx
                             , _varTypename = t' ^. typename
                             }
        entry = overloads . (at typeIdx)

    scopeUse . freeVarIdx %= (+ 1)
    entry .= Just entryVal
    return entryVal

  nameLookupV (name, tpat) = do
    scopeStack' <- use scopeStack
    case tpat of
      Exact t -> do
        rt <- nameLookupT t
        let typeIdx = rt ^. typeID
        found <- mapM (exactLookupV typeIdx) scopeStack'
        let Just closest = msum found
        return closest
      InferReturn targs -> do
        found <- mapM (inferLookupV targs) scopeStack'
        let all = concat found
        case all of
          [sole] -> return sole
   where
    exactLookupV typeIdx scopeIdx = do
      let scope     = scopes . (at scopeIdx) . assumeExists
          overrides = scope . vars . (at name) . (non Map.empty)
          entry     = overrides . (at typeIdx)
      use entry
    inferLookupV targs scopeIdx = do
      scope <- use $ scopes . (at scopeIdx) . assumeExists
      let overrides = scope ^. vars . (at name) . (non Map.empty)
          match     = \v -> case v ^. varTypename of
            FnTypename _ _ targs' -> targs == targs'
            otherwise             -> False
          matching = Map.elems $ Map.filter match overrides
      return matching

  leave = do
    scopeStack %= tail
