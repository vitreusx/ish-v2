{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import           Parser
import           ParserDef
import           System.Console.CmdArgs
import           System.Console.Haskeline
import           Text.Megaparsec         hiding ( State )
import           Control.Monad.Trans
import           Control.Monad.State.Strict
import           Data.Functor.Identity

data Args = FromFile { file' :: Maybe String } | ViaRepl
            deriving (Show, Data, Typeable)

file =
  FromFile { file' = def &= typ "FILE" &= args }
    &= auto
    &= explicit
    &= name "file"
    &= help "Load program from file or STDIN"

repl = ViaRepl &= explicit &= name "repl" &= help "Start REPL"

ish =
  (modes [file, repl])
    &= program "ish"
    &= help "Ish language interpreter/REPL"
    &= summary "Ish v0"

liftState :: State s a -> StateT s IO a
liftState x = StateT (\s' -> return $ runIdentity (runStateT x s'))

parseTestT
  :: Show a
  => Parser a
  -> String
  -> String
  -> StateT ParserState IO ()
parseTestT p name s = do
  v <- liftState $ runParserT p name s
  case v of
    Left  e -> lift (putStr $ errorBundlePretty e)
    Right x -> lift (print x)

fromFile :: Maybe String -> IO ()
fromFile f = do
  (source, name) <- case f of
    Nothing   -> (,) <$> getContents <*> (return "(stdin)")
    Just path -> (,) <$> readFile path <*> (return path)
  evalStateT (parseTestT pTopLevel name source) parserState0

viaRepl :: IO ()
viaRepl = do
  evalStateT (runInputT defaultSettings loop) parserState0
 where
  loop = do
    minput <- getInputLine "> "
    case minput of
      Nothing -> outputStrLn "Farewell."
      Just input ->
        (lift (parseTestT pTopLevel "(repl)" input)) >> loop

main :: IO ()
main = do
  args <- cmdArgs ish
  case args of
    FromFile { file' = f } -> fromFile f
    ViaRepl                -> viaRepl
