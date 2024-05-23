{-# LANGUAGE ImpredicativeTypes #-}

module Language.Hasmtlib.Monad where

import Language.Hasmtlib.Type.Expr
import Language.Hasmtlib.Type.SMT
import Data.Coerce
import Control.Monad.State
import qualified Data.Sequence as Seq

data SMT = SMT 
  { 
      lastAtom :: {-# UNPACK #-} !Int
    , intVars  :: Seq.Seq (SMTVar IntType)
    , realVars :: Seq.Seq (SMTVar RealType)
    , boolVars :: Seq.Seq (SMTVar BoolType)
    , formulas :: Seq.Seq (Expr BoolType) 
  } deriving Show

-- We can do better here: 
-- MonadState SMT m => m (SMTVar t) 
intVar :: MonadState SMT m => m (SMTVar IntType)
intVar = do
  (SMT la ivs rvs bvs fs) <- get
  let la' = la + 1
      newVar = coerce la'
  put $ SMT la' (ivs Seq.|> newVar) rvs bvs fs
  return newVar

realVar :: MonadState SMT m => m (SMTVar RealType)
realVar = do
  (SMT la ivs rvs bvs fs) <- get
  let la' = la + 1
      newVar = coerce la'
  put $ SMT la' ivs (rvs Seq.|> newVar) bvs fs
  return newVar

boolVar :: MonadState SMT m => m (SMTVar BoolType)
boolVar = do
  (SMT la ivs rvs bvs fs) <- get
  let la' = la + 1
      newVar = coerce la'
  put $ SMT la' ivs rvs (bvs Seq.|> newVar) fs
  return newVar

assert :: MonadState SMT m => Expr BoolType -> m ()
assert expr = modify $ \s -> s { formulas = formulas s Seq.|> expr } 