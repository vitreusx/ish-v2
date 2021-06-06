module Layout where

import           Lexer
import           Parser
import           Control.Monad.State
import           Control.Lens
import           Text.Megaparsec

curIndent :: Parser IndentInfo
curIndent = do
  sp <- getSourcePos
  return (unPos $ sourceLine sp, unPos $ sourceColumn sp)

local' :: MonadState s m => m s -> m a -> m a
local' f x = do
  prior <- get
  new   <- f
  put new
  v <- x
  put prior
  return v

withPos :: Parser a -> Parser a
withPos x = do
  local' (refIndent <~ curIndent >> get) x

withBlock :: (a -> [b] -> c) -> Parser a -> Parser b -> Parser c
withBlock f a p = withPos (withBlock' f a p)

withBlock' :: (a -> [b] -> c) -> Parser a -> Parser b -> Parser c
withBlock' f a p = do
  r1 <- a <* scn
  r2 <- sameOrIndented >> block p
  return (f r1 r2)

indented :: Parser ()
indented = do
  refCol <- uses refIndent snd
  curCol <- snd <$> curIndent

  let ord = compare refCol curCol
  when (ord /= LT) $ do
    let err = IncorrectIndent LT refCol curCol
    throwParseError err

same :: Parser ()
same = do
  refLine <- uses refIndent fst
  curLine <- fst <$> curIndent

  when (refLine /= curLine) $ do
    let err = IncorrectLine refLine curLine
    throwParseError err

sameOrIndented :: Parser ()
sameOrIndented = indented <|> same

checkIndent :: Int -> Parser ()
checkIndent ref = do
  curCol <- snd <$> curIndent

  let ord = compare ref curCol
  when (ord /= EQ) $ do
    let err = IncorrectIndent EQ ref curCol
    throwParseError err

block :: Parser a -> Parser [a]
block p = do
  blockRef <- snd <$> curIndent
  some (checkIndent blockRef >> p <* scn)

topLevel :: Parser a -> Parser a
topLevel p = checkIndent 1 >> p
