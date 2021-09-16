module Eval where

import           Parser
import           Syntax                  hiding ( Return
                                                , Break
                                                , Continue
                                                )
import qualified Syntax                        as Syn
import           VM
import           Control.Lens
import           Control.Monad.Cont
import           Control.Monad.Except
import           Control.Monad.State
import           Data.Maybe

evalTL :: TopLevel -> Eval ()
evalTL (TopLevel _ stmts) = do
  mapM_ evalSt stmts

evalLit :: Expr -> Value -> TypePat -> Eval Value
evalLit e v t = do
  unless ((typeof v) `typeMatches` t) $ do
    throwError $ InvalidType e t
  return v

evalX :: Expr -> TypePat -> Eval Value

evalX e@(ELitInt _ n) t = do
  evalLit e (IntV n) t

evalX e@(ELitStr _ s) t = do
  evalLit e (StringV s) t

evalX e@(EKind _) t = do
  evalLit e Kind t

evalX e@(ERecur _) t = do
  vlens <- VM.lookup (ExactSym Recur) t
  use $ cloneLens vlens

evalX (EVar (Ident _ x)) t = do
  vlens <- VM.lookup (FromName x) t
  use $ cloneLens vlens

evalX (EApp _ f args) t = do
  vargs <- mapM (\a -> evalX a AnyType) args
  let ft = FnPat (map typeof vargs)

  fv'        <- evalX f ft
  typecheck' <- use $ curEnv . typecheck
  if typecheck' || isPhantom fv' || any isPhantom vargs
    then do
      let retType = case fv' of
            FnV     fv          -> let FnT ret _ = fv ^. fnType in ret
            Phantom (FnT ret _) -> ret
      return $ Phantom retType
    else do
      let FnV fv            = fv'
          FnT ret argtypes' = fv ^. fnType
      case fv ^. fnDef of
        Defined (EFnDecl _ _ fargs fbody) -> do
          let argnames = [ name | (name, t) <- fargs ]
          scoped $ callCC $ \retcont -> do
            curEnv .= fv ^. closure
            declare Return (ReturnJ False ret retcont)
            declare Recur  fv'
            mapM_ (\(x, v) -> declare (Var x) v) (zip argnames vargs)
            mapM_ evalSt                         fbody
            return VoidV
        Intrinsic f -> f vargs

evalX (EPrefix r op e) t = evalX (EApp r (EVar op) [e]) t

evalX (EInfix r op e1 e2) t = evalX (EApp r (EVar op) [e1, e2]) t

evalX (EPostfix r op e) t = evalX (EApp r (EVar op) [e]) t

evalX decl@(EFnDecl _ declRetType args body) t = do
  argTypes      <- mapM (\(x, a) -> evalX a AnyType) args
  prelimRetType <- case declRetType of
    Nothing -> do
      case t of
        ExactType (FnT retType _) -> return $ Just retType
        _                         -> return Nothing
    Just declRetType' -> do
      rt <- evalX declRetType' AnyType
      unless (rt `typeMatches` t) $ do
        throwError $ InvalidType declRetType' t
      return $ Just rt

  let retCont = const $ return ()
  retJ <- case prelimRetType of
    Just rt -> do
      return $ ReturnJ False rt retCont
    Nothing -> do
      ptr <- malloc
      memory . (at ptr) .= Just VoidT
      return $ ReturnJ True (Ref ptr) retCont

  let ReturnJ _ retType _ = retJ

  finalRet <- local' (curEnv . typecheck .= True) $ scoped $ do
    declare Return retJ
    when (isJust prelimRetType) $ do
      declare Recur $ Phantom (FnT retType argTypes)

    mapM_ (\((x, _), t) -> declare (Var x) (Phantom t))
          (zip args argTypes)
    mapM_ evalSt body
    case retType of
      Ref ptr -> use $ memory . (at ptr) . assumeEx
      _       -> return retType

  let ft = FnT finalRet argTypes
  unless (ft `typeMatches` t) $ do
    throwError (InvalidType decl t)

  ctx <- use $ curEnv
  return $ FnV $ FnValue { _fnType  = ft
                         , _fnDef   = Defined decl
                         , _closure = ctx
                         }

evalX x@(EFnType _ ret args) t = do
  vret  <- evalX ret AnyType
  vargs <- mapM (\a -> evalX a AnyType) args
  let fnt = FnT vret vargs
  unless ((typeof fnt) `typeMatches` t) $ do
    throwError (InvalidType x t)
  return fnt

local' :: MonadState s m => m a -> m b -> m b
local' s x = do
  prior <- get
  s
  val <- x
  put prior
  return val

evalSt :: Stmt -> Eval ()

evalSt (Let    _ items) = mapM_ evalLet items

evalSt (Assign _ items) = mapM_ evalAssign items

evalSt (ExprStmt exprs) = mapM_ (\e -> evalX e AnyType) exprs

evalSt (If _ cond body) = do
  typecheck' <- use $ curEnv . typecheck
  if typecheck'
    then do
      evalX cond (ExactType BoolT)
      scoped $ mapM_ evalSt body
    else do
      BoolV vcond <- evalX cond (ExactType BoolT)
      when vcond $ scoped $ mapM_ evalSt body

evalSt (IfElse _ cond then' else') = do
  typecheck' <- use $ curEnv . typecheck
  if typecheck'
    then do
      evalX cond (ExactType BoolT)
      scoped $ mapM_ evalSt then'
      scoped $ mapM_ evalSt else'
    else do
      BoolV vcond <- evalX cond (ExactType BoolT)
      if vcond
        then scoped $ mapM_ evalSt then'
        else scoped $ mapM_ evalSt else'

evalSt (While _ cond body) = do
  scoped $ callCC $ \brk -> do
    declare Break $ Jump (brk ())
    typecheck' <- use $ curEnv . typecheck
    if typecheck'
      then do
        declare Continue $ Jump (brk ())
        evalX cond (ExactType BoolT)
        mapM_ evalSt body
      else do
        evalWhile cond body

evalSt (Syn.Break _) = do
  vlens      <- VM.lookup (ExactSym Break) (ExactType JumpT)
  Jump brk   <- use vlens

  typecheck' <- use $ curEnv . typecheck
  unless typecheck' $ brk

evalSt (Syn.Continue _) = do
  vlens      <- VM.lookup (ExactSym Continue) (ExactType JumpT)
  Jump cont  <- use $ vlens

  typecheck' <- use $ curEnv . typecheck
  unless typecheck' $ cont

evalSt x@(Syn.Return _ me) = do
  vlens <- VM.lookup (ExactSym Return) (ExactType JumpT)
  ReturnJ incomplete retType ret <- use vlens

  ve <- case me of
    Just e  -> evalX e AnyType
    Nothing -> return VoidV

  typecheck' <- use $ curEnv . typecheck
  if typecheck'
    then do
      if incomplete
        then do
          let Ref ptr = retType
          memory . (at ptr) . assumeEx .= typeof ve
          vlens2 <- VM.lookup (ExactSym Return) (ExactType JumpT)
          vlens2 .= ReturnJ False (typeof ve) ret
        else do
          retType' <- case retType of
            Ref ptr -> use $ memory . (at ptr) . assumeEx
            _       -> return retType

          unless ((typeof ve) `typeMatches` (ExactType retType')) $ do
            throwError $ InvalidReturn x retType'
    else do
      ret ve


evalSt (Future _ x t) = do
  vt <- evalX t AnyType
  declare (Var x) (Phantom vt)

evalWhile :: Expr -> [Stmt] -> Eval ()
evalWhile cond body = do
  BoolV vcond <- evalX cond (ExactType BoolT)
  when vcond $ do
    callCC $ \cont -> do
      declare Continue $ Jump (cont ())
      mapM_ evalSt body
    evalWhile cond body

evalLet :: LetItem -> Eval ()
evalLet (x, mt, e) = do
  t <- case mt of
    Nothing -> return AnyType
    Just t' -> ExactType <$> evalX t' AnyType
  ve <- evalX e t
  declare (Var x) ve

evalAssign :: AssignItem -> Eval ()
evalAssign (Ident _ x, e) = do
  ve    <- evalX e AnyType
  xlens <- VM.lookup (FromName x) (ExactType $ typeof ve)
  xlens .= ve
