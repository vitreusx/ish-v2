module Eval where

import           Syntax                  hiding ( Return
                                                , Break
                                                , Continue
                                                )
import qualified Syntax                        as Syn
import           VM
import           Control.Lens
import           Control.Monad.Cont
import           Control.Monad.Except

evalTL :: TopLevel -> Eval ()
evalTL (TopLevel _ stmts) = do
  scoped (mapM_ evalSt stmts)

evalX :: Expr -> Value -> Eval Value
evalX (ELitInt _ n) t = return (IntV n) >>= checkType t
evalX (ELitStr _ s) t = return (StringV s) >>= checkType t
evalX (EKind _    ) t = return Kind >>= checkType t

evalX (EVar  x    ) t = do
  vlens <- VM.lookup (Var x) t
  use $ cloneLens vlens

evalX (EApp _ f args) t = do
  vargs <- mapM (\a -> evalX a Any) args
  let ft = FnT Any (map typeof vargs)
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
  ret'     <- evalX ret Any
  argtypes <- mapM (\(argx, argt) -> evalX argt Any) args
  let ft = FnT ret' argtypes
  ctx <- use curEnv
  if ft `matches` t
    then return $ FnV $ FnValue { _fnType  = ft
                                , _fnDef   = Defined decl
                                , _closure = ctx
                                }
    else throwError InvalidType

evalX (ELam p ret args e) t =
  evalX (EFnDecl p ret args [Syn.Return p $ Just e]) t

evalX (EFnType _ ret args) t = case t of
  FnT ret' args' -> do
    vret  <- evalX ret ret'
    vargs <- mapM (\(a, t) -> evalX a t) (zip args args')
    return $ FnT vret vargs
  _ -> throwError InvalidType

evalSt :: Stmt -> Eval ()

evalSt (Let    _ items) = mapM_ evalLet items

evalSt (Assign _ items) = mapM_ evalAssign items

evalSt (ExprStmt exprs) = mapM_ (\e -> evalX e Any) exprs

evalSt (If _ cond body) = do
  BoolV vcond <- evalX cond BoolT
  when vcond $ scoped $ mapM_ evalSt body

evalSt (IfElse _ cond then' else') = do
  BoolV vcond <- evalX cond BoolT
  if vcond
    then scoped $ mapM_ evalSt then'
    else scoped $ mapM_ evalSt else'

evalSt (While _ cond body) = do
  scoped $ callCC $ \brk -> do
    declare Break $ Jump (brk ())
    evalWhile cond body

evalSt (Syn.Break _) = do
  vlens    <- VM.lookup Break JumpT
  Jump brk <- use $ cloneLens vlens
  brk

evalSt (Syn.Continue _) = do
  vlens     <- VM.lookup Continue JumpT
  Jump cont <- use $ cloneLens vlens
  cont

evalSt (Syn.Return _ me) = do
  vlens               <- VM.lookup Return JumpT
  ReturnJ ret retType <- use $ cloneLens vlens
  ve                  <- case me of
    Just e  -> evalX e retType
    Nothing -> do
      let v = VoidV
      if (typeof v) `matches` retType
        then return v
        else throwError InvalidType

  ret ve

evalSt (Future _ x t) = do
  vt <- evalX t AnyT
  declare (Var x) (Phantom vt)

evalWhile :: Expr -> [Stmt] -> Eval ()
evalWhile cond body = do
  BoolV vcond <- evalX cond BoolT
  when vcond $ do
    callCC $ \cont -> do
      declare Continue $ Jump (cont ())
      mapM_ evalSt body
      evalWhile cond body

evalLet :: LetItem -> Eval ()
evalLet (x, mt, e) = do
  t <- case mt of
    Nothing -> return Any
    Just t' -> evalX t' Any
  ve <- evalX e t
  declare (Var x) ve

evalAssign :: AssignItem -> Eval ()
evalAssign (x, e) = do
  ve    <- evalX e Any
  xlens <- VM.lookup (Var x) (typeof ve)
  (cloneLens xlens) .= ve
