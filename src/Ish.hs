-- |

module Ish where

import           Parser
import           ParserDefs
import           VM
import           Eval
import           Control.Monad.State
import           Text.Megaparsec         hiding ( State )
import           Text.Show.Pretty
import           Control.Monad.Except
import           Control.Monad.Cont
import           Control.Lens

type Ish = ContT () (StateT (ParserEnv, VM) IO)

liftState'
  :: (t -> s)
  -> (m (a, s) -> n (b, t))
  -> StateT s m a
  -> StateT t n b
liftState' p q (StateT x) = StateT (q . x . p)

setInitialPos :: SourcePos -> Parser ()
setInitialPos pos = do
  st <- getParserState
  let curPosState = statePosState st
      finPosState = curPosState { pstateSourcePos = pos }
      finState    = st { statePosState = curPosState }
  setParserState st

fromSource :: SourcePos -> String -> Ish ()
fromSource pos code = do
  (_, vm) <- get
  let f (parEnv, vm) = parEnv
      g x = do
        (ret, parEnv) <- x
        return (ret, (parEnv, vm))
      p    = setInitialPos pos >> pTopLevel
      path = sourceName pos

  v <- lift $ liftState' f g $ runParserT p path code
  case v of
    Left  e -> liftIO $ putStr $ errorBundlePretty e
    Right x -> liftEval (evalTL x)

liftCont'
  :: (n r -> m r) -> (m r -> n r) -> ContT r m a -> ContT r n a
liftCont' p q (ContT x) = ContT (\f -> q (x (p . f)))

liftEval :: Eval a -> Ish a
liftEval x = do
  (parEnv, _) <- get
  let pf (parEnv, vm) = vm
      pg x = do
        (ret, vm) <- x
        return (ret, (parEnv, vm))
      p = liftState' pf pg
      qf vm = (parEnv, vm)
      qg x = do
        (ret, (_, vm)) <- x
        return (ret, vm)
      q  = liftState' qf qg
      x' = do
        local' (curEnv . typecheck .= True) x
        x
  liftCont' q p x'

runIsh :: Ish () -> IO ()
runIsh x = do
  evalStateT (runContT x return) (mkParserEnv, mkVM)
