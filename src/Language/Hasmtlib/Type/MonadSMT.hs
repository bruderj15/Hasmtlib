module Language.Hasmtlib.Type.MonadSMT where

import Language.Hasmtlib.Internal.Expr
import Language.Hasmtlib.Type.Option
import Data.Proxy
import Control.Monad.State

class MonadState s m => MonadSMT s m where
  -- | Construct a variable.
  --   Usage:
  --      x :: Expr RealType <- var' (Proxy @RealType)
  var'       :: forall t. KnownSMTRepr t => Proxy t -> m (Expr t)

  -- | Assert a boolean expression.
  --   Usage
  --      x :: Expr IntType <- var @IntType
  --      assert $ x + 5 === 42
  assert    :: Expr BoolType -> m ()

  -- | Set an SMT-Solver-Option.
  setOption :: SMTOption -> m ()

  -- | Set the logic for the SMT-Solver to use.
  --   Usage:
  --      setLogic "QF_LRA"
  setLogic  :: String -> m ()

-- | Wrapper for @var'@ which hides the Proxy
var :: forall t s m. (KnownSMTRepr t, MonadSMT s m) => m (Expr t)
var = var' (Proxy @t)
{-# INLINE var #-}

-- | Create a constant.
--   Usage
--      >>> constant True
--          Constant (BoolValue True)
--
--      >>> let x :: Integer = 10 ; constant x
--          Constant (IntValue 10)
--
--      >>> constant @IntType 5
--          Constant (IntValue 5)
--
--      >>> constant @(BvType 8) 5
--          Constant (BvValue 0000101)
constant :: KnownSMTRepr t => ValueType t -> Expr t
constant = Constant . putValue
{-# INLINE constant #-}
