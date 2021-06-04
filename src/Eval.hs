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

evalTL :: TopLevel -> Eval ()
evalTL (TopLevel _ stmts) = do
  mapM_ evalSt stmts

evalLit :: Expr -> Value -> TypePat -> Eval Value
evalLit e v t = do
  if (typeof v) `typeMatches` t
    then return v
    else throwError (InvalidType e t)

evalX :: Expr -> TypePat -> Eval Value

evalX e@(ELitInt _ n) t = do
  evalLit e (IntV n) t

evalX e@(ELitStr _ s) t = do
  evalLit e (StringV s) t

evalX e@(EKind _) t = do
  evalLit e Kind t

evalX (EVar (Ident _ x)) t = do
  vlens <- VM.lookup (FromName x) t
  use $ cloneLens vlens

evalX (EApp _ f args) t = do
  vargs <- mapM (\a -> evalX a AnyType) args
  let ft = FnPat (map typeof vargs)
  FnV fv <- evalX f ft
  let FnT ret' argtypes' = fv ^. fnType

  case fv ^. fnDef of
    Defined (EFnDecl _ fret fargs fbody) -> do
      let argnames = [ name | (name, t) <- fargs ]
      scoped $ callCC $ \retcont -> do
        curEnv .= fv ^. closure
        declare Return (ReturnJ retcont ret')
        mapM_ (\(x, v) -> declare (Var x) v) (zip argnames vargs)
        mapM_ evalSt                         fbody
        return VoidV
    Intrinsic f -> f vargs

evalX (EPrefix r op e) t = evalX (EApp r (EVar op) [e]) t
evalX (EInfix r op e1 e2) t = evalX (EApp r (EVar op) [e1, e2]) t
evalX (EPostfix r op e) t = evalX (EApp r (EVar op) [e]) t

evalX decl@(EFnDecl _ ret args body) t = do
  ret'     <- evalX ret AnyType
  argtypes <- mapM (\(argx, argt) -> evalX argt AnyType) args
  let ft = FnT ret' argtypes
  ctx <- use curEnv
  if ft `typeMatches` t
    then return $ FnV $ FnValue { _fnType  = ft
                                , _fnDef   = Defined decl
                                , _closure = ctx
                                }
    else throwError (InvalidType decl t)

evalX (ELam p ret args e) t =
  evalX (EFnDecl p ret args [Syn.Return p $ Just e]) t

evalX x@(EFnType _ ret args) t = do
  vret  <- evalX ret AnyType
  vargs <- mapM (\a -> evalX a AnyType) args
  let fnt = FnT vret vargs
  if (typeof fnt) `typeMatches` t
    then return fnt
    else throwError (InvalidType x t)

evalSt :: Stmt -> Eval ()

evalSt (Let    _ items) = mapM_ evalLet items

evalSt (Assign _ items) = mapM_ evalAssign items

evalSt (ExprStmt exprs) = mapM_ (\e -> evalX e AnyType) exprs

evalSt (If _ cond body) = do
  BoolV vcond <- evalX cond (ExactType BoolT)
  when vcond $ scoped $ mapM_ evalSt body

evalSt (IfElse _ cond then' else') = do
  BoolV vcond <- evalX cond (ExactType BoolT)
  if vcond
    then scoped $ mapM_ evalSt then'
    else scoped $ mapM_ evalSt else'

evalSt (While _ cond body) = do
  scoped $ callCC $ \brk -> do
    declare Break $ Jump (brk ())
    evalWhile cond body

evalSt (Syn.Break _) = do
  vlens    <- VM.lookup (ExactSym Break) (ExactType JumpT)
  Jump brk <- use vlens
  brk

evalSt (Syn.Continue _) = do
  vlens     <- VM.lookup (ExactSym Continue) (ExactType JumpT)
  Jump cont <- use $ vlens
  cont

evalSt x@(Syn.Return _ me) = do
  vlens               <- VM.lookup (ExactSym Return) (ExactType JumpT)
  ReturnJ ret retType <- use vlens

  ve                  <- case me of
    Just e  -> evalX e (ExactType retType)
    Nothing -> do
      if retType == VoidT
        then return VoidV
        else throwError (NonVoidReturn retType)

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
