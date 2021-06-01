{-# LANGUAGE QuasiQuotes #-}

module Intrin where

import           VM
import           Syntax
import           NeatInterpolation
import           IshDef
import           Text.Megaparsec.Pos
import           Control.Lens
import           Data.Text
import           Control.Monad.IO.Class

pIntrin :: Ish ()
pIntrin = fromSource (initialPos "intrin") decls
 where
  decls = unpack [text|
// Intrinsic types
future int: *
future bool: *
future string: *
future void: *

// Operators
// Precedence levels taken from C++
// Lower number -> higher precedence
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

// Intrinsic functions
future show: fn string(int)
future show: fn string(bool)
future print: fn void(string)
future error: fn void(string)
|]

intrinT :: String -> Value -> Eval ()
intrinT x t = do
  xlens <- VM.lookup (Var $ IntrinName x) Kind
  cloneLens xlens .= t

intrinF :: String -> Value -> ([Value] -> Eval Value) -> Eval ()
intrinF x t f = do
  xlens <- VM.lookup (Var (IntrinName x)) t
  let fnv = FnValue { _fnType  = t
                    , _fnDef   = Intrinsic f
                    , _closure = env0
                    }
  cloneLens xlens .= FnV fnv

evalIntrin :: Eval ()
evalIntrin = do
  intrinT "int"    IntT
  intrinT "bool"   BoolT
  intrinT "string" StringT
  intrinT "void"   VoidT

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
  intrinF "-" (FnT IntT [IntT])
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
    $ \[StringV s] -> liftIO $ print s >> return VoidV
  intrinF "error" (FnT VoidT [StringT])
    $ \[StringV s] -> liftIO $ fail s >> return VoidV

intrin :: Ish ()
intrin = do
  pIntrin
  liftEval evalIntrin
