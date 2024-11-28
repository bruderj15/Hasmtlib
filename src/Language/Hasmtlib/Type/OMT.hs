{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE LambdaCase #-}

{- |
This module provides a concrete implementation for 'MonadOMT' with it's state 'OMT'.
-}
module Language.Hasmtlib.Type.OMT
(
  -- * SoftFormula
  -- ** Type
  SoftFormula(..)

  -- ** Lens
, formula, mWeight, mGroupId

  -- * Optimization targets
, Minimize(..), Maximize(..)

  -- * OMT
  -- ** Type
, OMT(..)

  -- ** Lens
, smt, targetMinimize, targetMaximize, softFormulas
)
where

import Language.Hasmtlib.Internal.Sharing
import Language.Hasmtlib.Type.MonadSMT
import Language.Hasmtlib.Type.SMTSort
import Language.Hasmtlib.Type.Expr
import Language.Hasmtlib.Type.SMT
import Data.Some.Constraint
import Data.List (isPrefixOf)
import Data.Default
import Data.Coerce
import Data.Sequence hiding ((|>), filter)
import Control.Monad.State
import Control.Lens hiding (List)

-- | An assertion of a booolean expression in OMT that may be weighted.
data SoftFormula = SoftFormula
  { _formula  :: Expr BoolSort    -- ^ The underlying soft formula
  , _mWeight  :: Maybe Double     -- ^ Weight of the soft formula
  , _mGroupId :: Maybe String     -- ^ Group-Id of the soft formula
  }
$(makeLenses ''SoftFormula)

-- | A newtype for numerical expressions that are target of a minimization.
newtype Minimize t = Minimize { _targetMin :: Expr t }

-- | A newtype for numerical expressions that are target of a maximization.
newtype Maximize t = Maximize { _targetMax :: Expr t }

-- | The state of the OMT-problem.
data OMT = OMT
  { _smt            :: !SMT                                 -- ^ The underlying 'SMT'-Problem
  , _targetMinimize :: !(Seq (SomeKnownSMTSort Minimize))   -- ^ All expressions to minimize
  , _targetMaximize :: !(Seq (SomeKnownSMTSort Maximize))   -- ^ All expressions to maximize
  , _softFormulas   :: !(Seq SoftFormula)                   -- ^ All soft assertions of boolean expressions
  }
$(makeLenses ''OMT)

instance Default OMT where
  def = OMT def mempty mempty mempty

instance Sharing OMT where
  type SharingMonad OMT = Monad
  stableMap = smt.Language.Hasmtlib.Type.SMT.stableMap
  assertSharedNode _ expr = modifying (smt.formulas) (|> expr)
  setSharingMode sm = smt.sharingMode .= sm

instance MonadState OMT m => MonadSMT OMT m where
  smtvar' _ = fmap coerce $ (smt.lastVarId) <+= 1
  {-# INLINE smtvar' #-}

  var' p = do
    newVar <- smtvar' p
    smt.vars %= (|> Some1 newVar)
    return $ Var newVar
  {-# INLINE var' #-}

  assert expr = do
    omt <- get
    sExpr <- runSharing (omt^.smt.sharingMode) expr
    qExpr <- case omt^.smt.mlogic of
      Nothing    -> return sExpr
      Just logic -> if "QF" `isPrefixOf` logic then return sExpr else quantify sExpr
    modify $ \s -> s & (smt.formulas) %~ (|> qExpr)
  {-# INLINE assert #-}

  setOption opt = smt.options <>= pure opt

  setLogic l = smt.mlogic ?= l

instance MonadSMT OMT m => MonadOMT OMT m where
  minimize expr = do
    sm <- use (smt.sharingMode)
    sExpr <- runSharing sm expr
    modifying targetMinimize (|> Some1 (Minimize sExpr))
  maximize expr = do
    sm <- use (smt.sharingMode)
    sExpr <- runSharing sm expr
    modifying targetMaximize (|> Some1 (Maximize sExpr))
  assertSoft expr w gid = do
    sm <- use (smt.sharingMode)
    sExpr <- runSharing sm expr
    modifying softFormulas (|> SoftFormula sExpr w gid)
