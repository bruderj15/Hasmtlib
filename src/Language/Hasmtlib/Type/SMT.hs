module Language.Hasmtlib.Type.SMT where

import Language.Hasmtlib.Type.Expr
import Data.Coerce
import Data.Default
import Control.Monad.State
import Data.Sequence

data SMT = SMT
  { lastAtom :: {-# UNPACK #-} !Int
  , intVars  :: Seq (SMTVar IntType)
  , realVars :: Seq (SMTVar RealType)
  , boolVars :: Seq (SMTVar BoolType)
  , formulas :: Seq (Expr BoolType)
  } deriving Show

instance Default SMT where
  def = SMT 0 mempty mempty mempty mempty

-- We can do better here:
-- MonadState SMT m => m (SMTVar t)
intVar :: MonadState SMT m => m (Expr IntType)
intVar = do
  (SMT la ivs rvs bvs fs) <- get
  let la' = la + 1
      newVar = coerce la'
  put $ SMT la' (ivs |> newVar) rvs bvs fs
  return $ Var newVar

realVar :: MonadState SMT m => m (Expr RealType)
realVar = do
  (SMT la ivs rvs bvs fs) <- get
  let la' = la + 1
      newVar = coerce la'
  put $ SMT la' ivs (rvs |> newVar) bvs fs
  return $ Var newVar

boolVar :: MonadState SMT m => m (Expr BoolType)
boolVar = do
  (SMT la ivs rvs bvs fs) <- get
  let la' = la + 1
      newVar = coerce la'
  put $ SMT la' ivs rvs (bvs |> newVar) fs
  return $ Var newVar

assert :: MonadState SMT m => Expr BoolType -> m ()
assert expr = modify $ \s -> s { formulas = formulas s |> expr }
