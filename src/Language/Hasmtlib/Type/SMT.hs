{-# LANGUAGE TemplateHaskell #-}

module Language.Hasmtlib.Type.SMT where

import Language.Hasmtlib.Type.Expr
import Data.Default
import Data.Sequence
import Control.Monad.State
import Control.Lens hiding ((|>))

data SMT = SMT
  { _lastAtom :: {-# UNPACK #-} !Int
  , _intVars  :: Seq (SMTVar IntType)
  , _realVars :: Seq (SMTVar RealType)
  , _boolVars :: Seq (SMTVar BoolType)
  , _formulas :: Seq (Expr BoolType)
  , _mlogic    :: Maybe String
  } deriving Show

$(makeLenses ''SMT)

instance Default SMT where
  def = SMT 0 mempty mempty mempty mempty Nothing

var :: forall m t. (MonadState SMT m, KnownSMTRepr t) => m (Expr t)
var = do
  smt <- get
  let la' = smt^.lastAtom + 1
      newVar = SMTVar la' Nothing
  put _
  return $ Var newVar

-- We can do better here:
-- MonadState SMT m => m (SMTVar t)
intVar :: MonadState SMT m => m (Expr IntType)
intVar = do
  smt <- get
  let la' = smt^.lastAtom + 1
      newVar = SMTVar la' Nothing
  modify $ \s -> s & intVars %~ (|> newVar)
  return $ Var newVar

realVar :: MonadState SMT m => m (Expr RealType)
realVar = do
  smt <- get
  let la' = smt^.lastAtom + 1
      newVar = SMTVar la' Nothing
  modify $ \s -> s & realVars %~ (|> newVar)
  return $ Var newVar

boolVar :: MonadState SMT m => m (Expr BoolType)
boolVar = do
  smt <- get
  let la' = smt^.lastAtom + 1
      newVar = SMTVar la' Nothing
  modify $ \s -> s & boolVars %~ (|> newVar)
  return $ Var newVar
  
setLogic :: MonadState SMT m => String -> m ()
setLogic l = modify $ \s -> s & mlogic ?~ l 

assert :: MonadState SMT m => Expr BoolType -> m ()
assert expr = modify $ \s -> s & formulas %~ (|> expr)

checkSat :: MonadState SMT m => m ()
checkSat = _