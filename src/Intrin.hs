{-# LANGUAGE QuasiQuotes #-}

module Intrin where

import           VM
import           Syntax
import           Quote

intrinDecls :: TopLevel
intrinDecls = [ishV2|future|]
