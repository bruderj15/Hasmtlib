{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Hasmtlib.Type.SMT where

import Language.Hasmtlib.Internal.Expr
import Language.Hasmtlib.Internal.Render
import Language.Hasmtlib.Type.MonadSMT
import Language.Hasmtlib.Type.SMTSort
import Language.Hasmtlib.Type.Option
import Data.List (isPrefixOf)
import Data.Default
import Data.Coerce
import Data.Sequence hiding ((|>), filter)
import Data.Data (toConstr, showConstr)
import Data.ByteString.Builder
import Control.Monad.State
import Control.Lens hiding (List)

-- | The state of the SMT-problem.
data SMT = SMT
  { _lastVarId :: {-# UNPACK #-} !Int                     -- ^ Last Id assigned to a new var
  , _vars     :: !(Seq (SomeKnownSMTSort SMTVar))         -- ^ All constructed variables
  , _formulas :: !(Seq (Expr BoolSort))                   -- ^ All asserted formulas
  , _mlogic   :: Maybe String                             -- ^ Logic for the SMT-Solver
  , _options  :: [SMTOption]                              -- ^ All manually configured SMT-Solver-Options
  }
$(makeLenses ''SMT)

instance Default SMT where
  def = SMT 0 mempty mempty mempty [ProduceModels True]

instance MonadState SMT m => MonadSMT SMT m where
  smtvar' _ = fmap coerce $ lastVarId <+= 1
  {-# INLINE smtvar' #-}

  var' p = do
    newVar <- smtvar' p
    vars %= (|> SomeSMTSort newVar)
    return $ Var newVar
  {-# INLINEABLE var' #-}

  assert expr = do
    smt <- get
    qExpr <- case smt^.mlogic of
      Nothing    -> return expr
      Just logic -> if "QF" `isPrefixOf` logic then return expr else quantify expr
    modify $ \s -> s & formulas %~ (|> qExpr)
  {-# INLINE assert #-}

  setOption opt = options %= ((opt:) . filter (not . eqCon opt))
    where
      eqCon :: SMTOption -> SMTOption -> Bool
      eqCon l r = showConstr (toConstr l) == showConstr (toConstr r)

  setLogic l = mlogic ?= l

instance RenderSeq SMT where
  renderSeq smt =
       fromList (render <$> smt^.options)
       >< maybe mempty (singleton . renderSetLogic . stringUtf8) (smt^.mlogic)
       >< renderVars (smt^.vars)
       >< fmap renderAssert (smt^.formulas)

renderSetLogic :: Builder -> Builder
renderSetLogic = renderUnary "set-logic"

renderDeclareVar :: forall t. KnownSMTSort t => SMTVar t -> Builder
renderDeclareVar v = renderTernary "declare-fun" v ("()" :: Builder) (sortSing @t)
{-# INLINEABLE renderDeclareVar #-}

renderAssert :: Expr BoolSort -> Builder
renderAssert = renderUnary "assert"
{-# INLINEABLE renderAssert #-}

renderVars :: Seq (SomeKnownSMTSort SMTVar) -> Seq Builder
renderVars = fmap (\(SomeSMTSort v) -> renderDeclareVar v)
{-# INLINEABLE renderVars #-}
