{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import           System.Console.CmdArgs
import           System.Console.Haskeline
import           IshDef
import           Intrin
import           Text.Megaparsec.Pos

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

fromFile :: Maybe String -> IO ()
fromFile f = do
  (source, name) <- case f of
    Nothing   -> (,) <$> getContents <*> (return "(stdin)")
    Just path -> (,) <$> readFile path <*> (return path)
  let x = do
        intrin
        fromSource (initialPos name) source
  runIsh x

-- viaRepl :: IO ()
-- viaRepl = do
--   runIsh (runInputT defaultSettings loop)
--  where
--   loop = do
--     minput <- getInputLine "> "
--     case minput of
--       Nothing -> outputStrLn "Farewell."
--       Just input ->
--         (lift (fromSource (initialPos "repl") input)) >> loop

main :: IO ()
main = do
  args <- cmdArgs ish
  case args of
    FromFile { file' = f } -> fromFile f
    -- ViaRepl                -> viaRepl
