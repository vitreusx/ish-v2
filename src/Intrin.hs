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
import           Control.Monad.State

intrinV :: String -> Value -> Ish ()
intrinV x t = liftEval $ do
  xlens <- VM.lookup (FromName x) AnyType
  xlens .= t

varFutures :: String
varFutures = unpack [text|
future int: *
future bool: *
future true: bool
future false: bool
future string: *
future void: *
|]

intrinVars :: Ish ()
intrinVars = do
  fromSource (initialPos "intrin-vars") varFutures

  intrinV "int"    IntT
  intrinV "bool"   BoolT
  intrinV "true"   (BoolV True)
  intrinV "false"  (BoolV False)
  intrinV "string" StringT
  intrinV "void"   VoidT


fnFutures :: String
fnFutures = unpack [text|
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
future typeof: fn string(string)
future valof: fn string(string)
|]

intrinF :: String -> Value -> ([Value] -> Eval Value) -> Ish ()
intrinF x t f = liftEval $ do
  let
    fnv =
      FnValue { _fnType = t, _fnDef = Intrinsic f, _closure = mkEnv }
  xlens <- VM.lookup (FromName x) (ExactType t)
  xlens .= FnV fnv

intrinFns :: Ish ()
intrinFns = do
  fromSource (initialPos "intrin-fn") fnFutures

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
  intrinF "typeof" (FnT StringT [StringT]) $ \[StringV x] -> do
    vlens <- VM.lookup (FromName x) AnyType
    v     <- use $ cloneLens vlens
    return $ StringV (show $ typeof v)
  intrinF "valof" (FnT StringT [StringT]) $ \[StringV x] -> do
    vlens <- VM.lookup (FromName x) AnyType
    v     <- use $ cloneLens vlens
    return $ StringV (show v)

intrin :: Ish ()
intrin = do
  intrinVars
  intrinFns
