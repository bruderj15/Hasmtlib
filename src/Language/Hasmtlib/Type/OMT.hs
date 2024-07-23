{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE LambdaCase #-}

module Language.Hasmtlib.Type.OMT where

import Language.Hasmtlib.Internal.Expr
import Language.Hasmtlib.Internal.Render
import Language.Hasmtlib.Type.MonadSMT
import Language.Hasmtlib.Type.Option
import Language.Hasmtlib.Type.SMT
import Data.List (isPrefixOf)
import Data.Default
import Data.Coerce
import Data.Sequence hiding ((|>), filter)
import Data.Data (toConstr, showConstr)
import Control.Monad.State
import Control.Lens hiding (List)

-- | An assertion of a booolean expression in OMT that may be weighted.
data SoftFormula = SoftFormula
  { _formula  :: Expr BoolSort
  , _mWeight  :: Maybe Double
  , _mGroupId :: Maybe String
  } deriving Show
$(makeLenses ''SoftFormula)

-- | A newtype for numerical expressions that are target of a minimization.
newtype Minimize t = Minimize { _targetMin :: Expr t }

-- | A newtype for numerical expressions that are target of a maximization.
newtype Maximize t = Maximize { _targetMax :: Expr t }

-- | The state of the OMT-problem.
data OMT = OMT
  { _smt            :: SMT                                  -- ^ The underlying 'SMT'-Problem
  , _targetMinimize :: !(Seq (SomeKnownSMTSort Minimize))   -- ^ All expressions to minimize
  , _targetMaximize :: !(Seq (SomeKnownSMTSort Maximize))   -- ^ All expressions to maximize
  , _softFormulas   :: !(Seq SoftFormula)                   -- ^ All soft assertions of boolean expressions
  }
$(makeLenses ''OMT)

instance Default OMT where
  def = OMT def mempty mempty mempty

instance MonadState OMT m => MonadSMT OMT m where
  smtvar' _ = fmap coerce $ (smt.lastVarId) <+= 1
  {-# INLINE smtvar' #-}

  var' p = do
    newVar <- smtvar' p
    smt.vars %= (|> SomeSMTSort newVar)
    return $ Var newVar
  {-# INLINE var' #-}

  assert expr = do
    omt <- get
    qExpr <- case omt^.smt.mlogic of
      Nothing    -> return expr
      Just logic -> if "QF" `isPrefixOf` logic then return expr else quantify expr
    modify $ \s -> s & (smt.formulas) %~ (|> qExpr)
  {-# INLINE assert #-}

  setOption opt = smt.options %= ((opt:) . filter (not . eqCon opt))
    where
      eqCon :: SMTOption -> SMTOption -> Bool
      eqCon l r = showConstr (toConstr l) == showConstr (toConstr r)

  setLogic l = smt.mlogic ?= l

instance MonadSMT OMT m => MonadOMT OMT m where
  minimize expr = targetMinimize %= (|> SomeSMTSort (Minimize expr))
  maximize expr = targetMaximize %= (|> SomeSMTSort (Maximize expr))
  assertSoft expr w gid = softFormulas %= (|> SoftFormula expr w gid)

instance Render SoftFormula where
  render sf = "(assert-soft " <> render (sf^.formula) <> " :weight " <> maybe "1" render (sf^.mWeight) <> renderGroupId (sf^.mGroupId) <> ")"
    where
      renderGroupId Nothing = mempty
      renderGroupId (Just groupId) = " :id " <> render groupId

instance KnownSMTSort t => Render (Minimize t) where
  render (Minimize expr) = "(minimize " <> render expr <> ")"

instance KnownSMTSort t => Render (Maximize t) where
  render (Maximize expr) = "(maximize " <> render expr <> ")"

instance RenderSeq OMT where
  renderSeq omt =
       renderSeq (omt^.smt)
    <> fmap render (omt^.softFormulas)
    <> fmap (\case SomeSMTSort minExpr -> render minExpr) (omt^.targetMinimize)
    <> fmap (\case SomeSMTSort maxExpr -> render maxExpr) (omt^.targetMaximize)
