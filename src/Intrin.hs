{-# LANGUAGE QuasiQuotes #-}

module Intrin where

import           VM
import           Parser
import           NeatInterpolation
import           Ish
import           Text.Megaparsec.Pos
import           Control.Lens
import           Data.Text
import           Control.Monad.IO.Class

intrinT :: String -> Value -> Ish ()
intrinT x t = liftEval $ do
  xlens <- VM.lookup (FromName x) AnyType
  xlens .= t

intrinF :: String -> Value -> ([Value] -> Eval Value) -> Ish ()
intrinF x t f = liftEval $ do
  xlens <- VM.lookup (FromName x) (ExactType t)
  let
    fnv =
      FnValue { _fnType = t, _fnDef = Intrinsic f, _closure = mkEnv }
  xlens .= FnV fnv

typeFutures :: String
typeFutures = unpack [text|
future int: *
future bool: *
future string: *
future void: *
|]

intrinTypes :: Ish ()
intrinTypes = do
  fromSource (initialPos "intrin-types") typeFutures

  intrinT "int"    IntT
  intrinT "bool"   BoolT
  intrinT "string" StringT
  intrinT "void"   VoidT

funFutures :: String
funFutures = unpack [text|
#[op 3  prefix  +  ]
future `+`:  fn int(int)
#[op 3  prefix  -  ]
future `-`:  fn int(int)
#[op 5  infixl  *  ]
future `*`:  fn int(int, int)
#[op 5  infixl  /  ]
future `/`:  fn int(int, int)
#[op 5  infixl  %  ]
future `%`:  fn int(int, int)
#[op 6  infixl  +  ]
future `+`:  fn int(int, int)
future `+`:  fn string(string, string)
#[op 6  infixl  -  ]
future `-`:  fn int(int, int)
#[op 9  infixl  <  ]
future `<`:  fn bool(int, int)
#[op 9  infixl  <= ]
future `<=`: fn bool(int, int)
#[op 9  infixl  >  ]
future `>`:  fn bool(int, int)
#[op 9  infixl  >= ]
future `>=`: fn bool(int, int)
#[op 10 infixl  == ]
future `==`: fn bool(int, int)
future `==`: fn bool(bool, bool)
future` ==`: fn bool(string, string)
#[op 10 infixl  != ]
future `!=`: fn bool(int, int)
future `!=`: fn bool(bool, bool)
future `!=`: fn bool(string, string)
#[op 14 infixl  && ]
future `&&`: fn bool(bool, bool)
#[op 15 infixl  || ]
future `||`: fn bool(bool, bool)

future show: fn string(int)
future show: fn string(bool)
future print: fn void(string)
future error: fn void(string)
|]

intrinFun :: Ish ()
intrinFun = do
  fromSource (initialPos "intrin-futures") funFutures

  intrinF "+" (FnT IntT [IntT]) $ \[IntV n] -> return $ IntV (n)
  intrinF "-" (FnT IntT [IntT]) $ \[IntV n] -> return $ IntV (-n)
  intrinF "*" (FnT IntT [IntT, IntT])
    $ \[IntV n, IntV m] -> return $ IntV (n * m)
  intrinF "/" (FnT IntT [IntT, IntT])
    $ \[IntV n, IntV m] -> return $ IntV (n `div` m)
  intrinF "%" (FnT IntT [IntT, IntT])
    $ \[IntV n, IntV m] -> return $ IntV (n `mod` m)
  intrinF "+" (FnT IntT [IntT, IntT])
    $ \[IntV n, IntV m] -> return $ IntV (n + m)
  intrinF "+" (FnT StringT [StringT, StringT])
    $ \[StringV s, StringV t] -> return $ StringV (s ++ t)
  intrinF "-" (FnT IntT [IntT, IntT])
    $ \[IntV n, IntV m] -> return $ IntV (n - m)
  intrinF "<" (FnT BoolT [IntT, IntT])
    $ \[IntV n, IntV m] -> return $ BoolV (n < m)
  intrinF "<=" (FnT BoolT [IntT, IntT])
    $ \[IntV n, IntV m] -> return $ BoolV (n <= m)
  intrinF ">" (FnT BoolT [IntT, IntT])
    $ \[IntV n, IntV m] -> return $ BoolV (n > m)
  intrinF ">=" (FnT BoolT [IntT, IntT])
    $ \[IntV n, IntV m] -> return $ BoolV (n >= m)
  intrinF "==" (FnT BoolT [IntT, IntT])
    $ \[IntV n, IntV m] -> return $ BoolV (n == m)
  intrinF "==" (FnT BoolT [BoolT, BoolT])
    $ \[BoolV p, BoolV q] -> return $ BoolV (p == q)
  intrinF "==" (FnT BoolT [StringT, StringT])
    $ \[StringV s, StringV t] -> return $ BoolV (s == t)
  intrinF "!=" (FnT BoolT [IntT, IntT])
    $ \[IntV n, IntV m] -> return $ BoolV (n /= m)
  intrinF "!=" (FnT BoolT [BoolT, BoolT])
    $ \[BoolV p, BoolV q] -> return $ BoolV (p /= q)
  intrinF "!=" (FnT BoolT [StringT, StringT])
    $ \[StringV s, StringV t] -> return $ BoolV (s /= t)
  intrinF "&&" (FnT BoolT [BoolT, BoolT])
    $ \[BoolV p, BoolV q] -> return $ BoolV (p && q)
  intrinF "||" (FnT BoolT [BoolT, BoolT])
    $ \[BoolV p, BoolV q] -> return $ BoolV (p || q)

  intrinF "show" (FnT StringT [IntT])
    $ \[IntV n] -> return $ StringV (show n)
  intrinF "show" (FnT StringT [BoolT])
    $ \[BoolV p] -> return $ StringV (show p)
  intrinF "print" (FnT VoidT [StringT])
    $ \[StringV s] -> liftIO $ putStr s >> return VoidV
  intrinF "error" (FnT VoidT [StringT])
    $ \[StringV s] -> liftIO $ fail s >> return VoidV

intrin :: Ish ()
intrin = do
  intrinTypes
  intrinFun
