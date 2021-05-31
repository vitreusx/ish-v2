-- |

module Quote
  ( ishV2
  ) where

import           Data.Generics
import qualified Language.Haskell.TH           as TH
import           Language.Haskell.TH.Quote
import           Text.Megaparsec
import           Control.Monad.State.Strict
import           Parser

ishV2 :: QuasiQuoter
ishV2 = QuasiQuoter { quoteExp  = ishExp
                    , quotePat  = ishPat
                    , quoteDec  = undefined
                    , quoteType = undefined
                    }

ishExp s = do
  x <- parseQQuotes s
  dataToExpQ (const Nothing) x

ishPat s = do
  x <- parseQQuotes s
  dataToPatQ (const Nothing) x

parseQQuotes s = do
  loc <- TH.location
  let file        = TH.loc_filename loc
      (line, col) = TH.loc_start loc
      p           = setPos (file, mkPos line, mkPos col) >> pTopLevel

  let res = evalState (runParserT p s file) parserState0
  case res of
    Left  e -> fail $ errorBundlePretty e
    Right x -> return x
