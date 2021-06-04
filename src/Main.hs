{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import           System.Console.CmdArgs
import           System.Console.Haskeline
import           Ish
import           Intrin
import           Text.Megaparsec.Pos
import           Control.Monad.State

data Args = FromFile
  { file' :: Maybe String
  }
  deriving (Show, Data, Typeable)

file =
  FromFile { file' = def &= typ "FILE" &= args }
    &= auto
    &= explicit
    &= name "file"
    &= help "Load program from file or STDIN"

ish =
  (modes [file])
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

main :: IO ()
main = do
  args <- cmdArgs ish
  case args of
    FromFile { file' = f } -> fromFile f
