{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Hasmtlib.Type.SMT where

import Language.Hasmtlib.Internal.Render
import Language.Hasmtlib.Internal.Uniplate1
import Language.Hasmtlib.Type.MonadSMT
import Language.Hasmtlib.Type.SMTSort
import Language.Hasmtlib.Type.Option
import Language.Hasmtlib.Type.Expr
import Data.GADT.Compare
import Data.GADT.DeepSeq
import Data.List (isPrefixOf)
import Data.Default
import Data.Coerce
import Data.Sequence hiding ((|>), filter)
import Data.Data (toConstr, showConstr)
import Data.ByteString.Builder
import Data.HashMap.Lazy (HashMap)
import Control.Monad.State
import Control.Lens hiding (List)
import System.Mem.StableName
import System.IO.Unsafe
import Unsafe.Coerce

-- | The state of the SMT-problem.
data SMT = SMT
  { _lastVarId :: {-# UNPACK #-} !Int                                 -- ^ Last Id assigned to a new var
  , _vars      :: !(Seq (SomeKnownSMTSort SMTVar))                    -- ^ All constructed variables
  , _formulas  :: !(Seq (Expr BoolSort))                              -- ^ All asserted formulas
  , _mlogic    :: Maybe String                                        -- ^ Logic for the SMT-Solver
  , _options   :: [SMTOption]                                         -- ^ All manually configured SMT-Solver-Options
  , _stableMap :: !(HashMap (StableName ()) (SomeKnownSMTSort Expr))  -- ^ Mapping between a 'StableName' and it's 'Expr' we may share
  }
$(makeLenses ''SMT)

instance Default SMT where
  def = SMT 0 mempty mempty mempty [ProduceModels True] mempty

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
    sExpr <- runSharing expr
    qExpr <- case smt^.mlogic of
      Nothing    -> return expr
      Just logic -> if "QF" `isPrefixOf` logic then return sExpr else quantify sExpr
    modify $ \s -> s & formulas %~ (|> qExpr)
  {-# INLINE assert #-}

  setOption opt = options %= ((opt:) . filter (not . eqCon opt))
    where
      eqCon :: SMTOption -> SMTOption -> Bool
      eqCon l r = showConstr (toConstr l) == showConstr (toConstr r)

  setLogic l = mlogic ?= l

-- | Shares all possible sub-expressions in given expression.
--   Replaces each node in the expression-tree with an auxiliary variable.
--   All nodes @x@ @y@ where @makeStableName x == makeStableName y@ are replaced with the same auxiliary variable.
--   Therfore this creates a DAG.
runSharing :: (KnownSMTSort t, MonadSMT SMT m) => Expr t -> m (Expr t)
runSharing = lazyParaM1 (
    \origExpr expr ->
      if isLeaf origExpr
      then return origExpr
      else case sortSing' origExpr of   -- scopes Equatable (Expr t) for specific t
        SBoolSort      -> share origExpr expr
        SIntSort       -> share origExpr expr
        SRealSort      -> share origExpr expr
        SBvSort _      -> share origExpr expr
        SArraySort _ _ -> share origExpr expr
        SStringSort    -> share origExpr expr)

-- | Returns an auxiliary variable representing this expression node.
--   If such a shared auxiliary variable exists already, returns that.
--   Otherwise creates one and returns it.
share :: (Equatable (Expr t), KnownSMTSort t, MonadSMT SMT m) => Expr t -> m (Expr t) -> m (Expr t)
share expr@(ForAll _ _) _ = return expr     -- sharing quantified expression would out-scope quantified var
share expr@(Exists _ _) _ = return expr
share origExpr expr = do
  let sn = unsafePerformIO (makeStableName' origExpr)
   in use (stableMap.at sn) >>= \mexpr' -> case mexpr' of
        Just (SomeSMTSort expr') -> case geq (sortSing' origExpr) (sortSing' expr') of
          Nothing -> expr >>= makeNode sn
          Just Refl -> return expr'
        Nothing -> expr >>= makeNode sn
  where
    makeNode :: (Equatable (Expr t), KnownSMTSort t, MonadSMT SMT m) => StableName () -> Expr t -> m (Expr t)
    makeNode sn nodeExpr = do
      nodeVar <- var
      modify $ \s -> s & formulas %~ (|> (nodeVar === nodeExpr))
      stableMap.at sn ?= SomeSMTSort nodeVar
      return nodeVar
    makeStableName' :: GNFData f => f a -> IO (StableName ())
    makeStableName' x = grnf x `seq` fmap unsafeCoerce (makeStableName x)

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
