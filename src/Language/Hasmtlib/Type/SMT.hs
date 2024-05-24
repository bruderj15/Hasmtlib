{-# LANGUAGE TemplateHaskell #-}

module Language.Hasmtlib.Type.SMT where

import Language.Hasmtlib.Type.Expr
import Data.Default
import Data.Sequence hiding ((|>))
import Control.Monad.State
import Control.Lens hiding (index)

data SMT = SMT
  { _lastAtom :: {-# UNPACK #-} !Int
  , _vars     :: Seq (SomeKnownSMTRepr SMTVar)
  , _formulas :: Seq (Expr BoolType)
  , _mlogic   :: Maybe String
  }

$(makeLenses ''SMT)

instance Default SMT where
  def = SMT 0 mempty mempty Nothing

var :: forall t m. (KnownSMTRepr t, MonadState SMT m) => m (Expr t)
var = do
  smt <- get
  let la' = smt^.lastAtom + 1
      newVar = SMTVar la' Nothing
  modify $ \s -> s & vars %~ (|> SomeKnownSMTRepr newVar) & lastAtom %~ (+1)
  return $ Var newVar

setLogic :: MonadState SMT m => String -> m ()
setLogic l = modify $ \s -> s & mlogic ?~ l

assert :: MonadState SMT m => Expr BoolType -> m ()
assert expr = modify $ \s -> s & formulas %~ (|> expr)

checkSat :: MonadState SMT m => m ()
checkSat = _