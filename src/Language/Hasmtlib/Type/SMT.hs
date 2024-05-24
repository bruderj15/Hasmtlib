{-# LANGUAGE TemplateHaskell #-}

module Language.Hasmtlib.Type.SMT where

import Language.Hasmtlib.Type.Expr
import Data.Default
import Data.Sequence hiding ((|>))
import Control.Monad.State
import Control.Lens hiding (index)

-- | SMT State
data SMT = SMT
  { _lastAtom :: {-# UNPACK #-} !Int
  , _vars     :: Seq (SomeKnownSMTRepr SMTVar)
  , _formulas :: Seq (Expr BoolType)
  , _mlogic   :: Maybe String
  }

$(makeLenses ''SMT)

instance Default SMT where
  def = SMT 0 mempty mempty Nothing

-- | Construct a variable
--   Usage:
--      x :: Expr RealType <- var @RealType
var :: forall t m. (KnownSMTRepr t, MonadState SMT m) => m (Expr t)
var = do
  smt <- get
  let la' = smt^.lastAtom + 1
      newVar = SMTVar la' Nothing
  modify $ \s -> s & vars %~ (|> SomeKnownSMTRepr newVar) & lastAtom %~ (+1)
  return $ Var newVar

-- | Set the logic for the SMT-Solver to use
--   Usage:
--      setLogic "QF_LRA"   
setLogic :: MonadState SMT m => String -> m ()
setLogic l = modify $ \s -> s & mlogic ?~ l

-- | Assert a boolean expression 
--   Usage 
--      x :: Expr IntType <- var @IntType
--      assert $ x + 5 === 42
assert :: MonadState SMT m => Expr BoolType -> m ()
assert expr = modify $ \s -> s & formulas %~ (|> expr)

checkSat :: MonadState SMT m => m ()
checkSat = _