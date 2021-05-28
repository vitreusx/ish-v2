module Indent where

import           ParserDef
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import           Text.Megaparsec         hiding ( State )
import           Control.Lens
import qualified Data.Set                      as Set
import           Text.Megaparsec.Pos
import           Text.Megaparsec.Char.Lexer

refIndent :: Parser IndentInfo
refIndent = view curRef <$> get

curIndent :: Parser IndentInfo
curIndent = do
  pos <- getSourcePos
  let line = unPos $ sourceLine pos
      col  = unPos $ sourceColumn pos
  return (line, col)

local' :: MonadState s m => (s -> s) -> m a -> m a
local' f x = do
  prior <- get
  put (f prior)
  v <- x
  put prior
  return v

withPos :: Parser a -> Parser a
withPos x = do
  ind <- curIndent
  local' (set curRef ind) x

withBlock :: (a -> [b] -> c) -> Parser a -> Parser b -> Parser c
withBlock f a p = withPos (withBlock' f a p)

withBlock' :: (a -> [b] -> c) -> Parser a -> Parser b -> Parser c
withBlock' f a p = do
  r1 <- a
  r2 <- sameOrIndented >> block p
  return (f r1 r2)

indented :: Parser ()
indented = do
  (_, refCol) <- refIndent
  (_, curCol) <- curIndent
  let ord = compare refCol curCol
  when (ord /= LT) $ incorrectIndent LT (mkPos refCol) (mkPos curCol)

same :: Parser ()
same = do
  (refLine, _) <- refIndent
  (curLine, _) <- curIndent
  when (refLine /= curLine)
    $ fancyFailure
    . Set.singleton
    $ ErrorCustom
    $ IncorrectLine refLine curLine

sameOrIndented :: Parser ()
sameOrIndented = indented <|> same

checkIndent :: Int -> Parser ()
checkIndent ref = do
  (_, curCol) <- curIndent
  let ord = compare ref curCol
  when (ord /= EQ) $ incorrectIndent EQ (mkPos ref) (mkPos curCol)

block :: Parser a -> Parser [a]
block p = do
  (_, refCol) <- curIndent
  some (checkIndent refCol >> p)

topLevel :: Parser a -> Parser a
topLevel p = checkIndent 1 >> p
