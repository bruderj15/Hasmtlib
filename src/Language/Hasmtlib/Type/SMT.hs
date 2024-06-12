{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Hasmtlib.Type.SMT
 ( SMT, lastVarId, vars, formulas, mlogic, options
 , var, constant
 , MonadSMT(..)
 , SMTOption(Incremental)
 , renderSMT, renderSetLogic, renderAssert, renderVars, renderDeclareVar
 )
 where

import Language.Hasmtlib.Internal.Expr
import Language.Hasmtlib.Internal.Render
import Data.Default
import Data.Coerce
import Data.Proxy
import Data.Sequence hiding ((|>), filter)
import Data.Data (Data, toConstr, showConstr)
import Data.ByteString.Builder
import Control.Monad.State
import Control.Lens hiding (List)

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

data SMTOption =
    PrintSuccess  Bool              -- | Print \"success\" after each operation
  | ProduceModels Bool              -- | Produce a satisfying assignment after each successful checkSat
  | Incremental   Bool              -- | Incremental solving
  deriving (Show, Eq, Ord, Data)

-- | SMT State
data SMT = SMT
  { _lastVarId :: {-# UNPACK #-} !Int             -- | Last Id assigned to a new var
  , _vars     :: !(Seq (SomeKnownSMTRepr SMTVar)) -- | All constructed variables
  , _formulas :: !(Seq (Expr BoolType))           -- | All asserted formulas
  , _mlogic   :: Maybe String                     -- | Logic for the SMT-Solver
  , _options  :: [SMTOption]                      -- | All manually configured SMT-Solver-Options
  }

instance Default SMT where
  def = SMT 0 mempty mempty mempty mempty

$(makeLenses ''SMT)

instance MonadState SMT m => MonadSMT SMT m where
  var' _ = do
    smt <- get
    let la' = smt^.lastVarId + 1
        newVar = coerce la'
    modify $ \s -> s & vars %~ (|> SomeKnownSMTRepr newVar) & lastVarId %~ (+1)
    return $ Var newVar
  {-# INLINEABLE var' #-}

  assert expr = modify $ \s -> s & formulas %~ (|> expr)
  {-# INLINE assert #-}

  setOption opt = options %= ((opt:) . filter (not . eqCon opt))
    where
      eqCon :: SMTOption -> SMTOption -> Bool
      eqCon l r = showConstr (toConstr l) == showConstr (toConstr r)

  setLogic l = mlogic ?= l

instance RenderSMTLib2 SMTOption where
  renderSMTLib2 (PrintSuccess  b) = renderBinary "set-option" (":print-success"  :: Builder) b
  renderSMTLib2 (ProduceModels b) = renderBinary "set-option" (":produce-models" :: Builder) b
  renderSMTLib2 (Incremental   b) = renderBinary "set-option" (":incremental"    :: Builder) b

renderSMT :: SMT -> Seq Builder
renderSMT smt =
     fromList (renderSMTLib2 <$> smt^.options)
  >< maybe mempty (singleton . renderSetLogic . stringUtf8) (smt^.mlogic)
  >< renderVars (smt^.vars)
  >< fmap renderAssert (smt^.formulas)

renderSetLogic :: Builder -> Builder
renderSetLogic = renderUnary "set-logic"

renderDeclareVar :: forall t. KnownSMTRepr t => SMTVar t -> Builder
renderDeclareVar v = renderTernary "declare-fun" v ("()" :: Builder) (singRepr @t)
{-# INLINEABLE renderDeclareVar #-}

renderAssert :: Expr BoolType -> Builder
renderAssert = renderUnary "assert"
{-# INLINEABLE renderAssert #-}

renderVars :: Seq (SomeKnownSMTRepr SMTVar) -> Seq Builder
renderVars = fmap (\(SomeKnownSMTRepr v) -> renderDeclareVar v)
{-# INLINEABLE renderVars #-}
