{-# LANGUAGE ImpredicativeTypes #-}

module Language.Hasmtlib.Monad where

import Language.Hasmtlib.Type.Expr
import Language.Hasmtlib.Type.SMT
import Data.Proxy
import Data.Coerce
import Control.Monad.State
import qualified Data.Set as Set

-- Strict...?
data SMT = SMT 
  { 
    lastAtom :: {-# UNPACK #-} !Int
  , intVars  :: Set.Set (SMTVar IntType)
  , realVars :: Set.Set (SMTVar RealType)
  , boolVars :: Set.Set (SMTVar BoolType)
  , formulas :: Set.Set (Expr BoolType) 
  }
  
-- Proxy required
var :: forall m t. MonadState SMT m => Proxy (t :: SMTType) -> m (SMTVar t)
var _ = do
  (SMT la vs fs) <- get
  let la' = la + 1
      newVar = coerce @Int @(SMTVar t) la'
  put $ SMT la' (Set.insert newVar vs) fs
  return newVar

--assert :: MonadState SMT m => 