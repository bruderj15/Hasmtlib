{-# LANGUAGE LambdaCase #-}

module Language.Hasmtlib.Type.MonadSMT where

import Language.Hasmtlib.Type.Expr
import Language.Hasmtlib.Type.Option
import Language.Hasmtlib.Type.SMTSort
import Language.Hasmtlib.Type.Solution
import Language.Hasmtlib.Codec
import Data.Proxy
import Control.Lens
import Control.Monad
import Control.Monad.State

-- | A 'MonadState' that holds an SMT-Problem.
class MonadState s m => MonadSMT s m where
  -- | Construct a variable.
  --   This is mainly intended for internal use.
  --   In the API use 'var'' instead.
  --
  -- @
  -- x :: SMTVar RealType <- smtvar' (Proxy @RealType)
  -- @
  smtvar' :: forall t. KnownSMTSort t => Proxy t -> m (SMTVar t)

  -- | Construct a variable as expression.
  --
  -- @
  -- x :: Expr RealType <- var' (Proxy @RealType)
  -- @
  var' :: forall t. KnownSMTSort t => Proxy t -> m (Expr t)

  -- | Assert a boolean expression.
  --
  -- @
  -- x :: Expr IntType <- var @IntType
  -- assert $ x + 5 === 42
  -- @
  assert :: Expr BoolSort -> m ()

  -- | Set an SMT-Solver-Option.
  --
  -- @
  -- setOption $ Incremental True
  -- @
  setOption :: SMTOption -> m ()

  -- | Set the logic for the SMT-Solver to use.
  --
  -- @
  -- setLogic \"QF_LRA\"
  -- @
  setLogic :: String -> m ()

-- | Wrapper for 'var'' which hides the 'Proxy'.
var :: forall t s m. (KnownSMTSort t, MonadSMT s m) => m (Expr t)
var = var' (Proxy @t)
{-# INLINE var #-}

-- | Wrapper for 'smtvar'' which hides the 'Proxy'.
--   This is mainly intended for internal use.
--   In the API use 'var' instead.
smtvar :: forall t s m. (KnownSMTSort t, MonadSMT s m) => m (SMTVar t)
smtvar = smtvar' (Proxy @t)
{-# INLINE smtvar #-}

-- | Create a constant.
--
--   >>> constant True
--       Constant (BoolValue True)
--
--   >>> let x :: Integer = 10 ; constant x
--       Constant (IntValue 10)
--
--   >>> constant @IntType 5
--       Constant (IntValue 5)
--
--   >>> constant @(BvType 8) 5
--       Constant (BvValue 0000101)
constant :: KnownSMTSort t => HaskellType t -> Expr t
constant = Constant . wrapValue
{-# INLINE constant #-}

-- | Maybe assert a boolean expression.
--   Asserts given expression if 'Maybe' is a 'Just'.
--   Does nothing otherwise.
assertMaybe :: MonadSMT s m => Maybe (Expr BoolSort) -> m ()
assertMaybe Nothing = return ()
assertMaybe (Just expr) = assert expr

--   We need this separate so we get a pure API for quantifiers
--   Ideally we would do that when rendering the expression
--   However Language.Hasmtlib.Internal.Render#render is pure but we need a new quantified var which is stateful
-- | Assign quantified variables to all quantified subexpressions of an expression.
--   This shall only be used internally.
--   Usually before rendering an assert.
quantify :: MonadSMT s m => KnownSMTSort t => Expr t -> m (Expr t)
quantify = transformM (
  \case (ForAll _ f) -> do
          qVar <- smtvar
          qBody <- quantify $ f $ Var qVar
          return $ ForAll (Just qVar) (const qBody)
        (Exists _ f) -> do
          qVar <- smtvar
          qBody <- quantify $ f $ Var qVar
          return $ Exists (Just qVar) (const qBody)
        expr -> return expr
  )

-- | A 'MonadSMT' that allows incremental solving.
class MonadSMT s m => MonadIncrSMT s m where
  -- | Push a new context (one) to the solvers context-stack.
  push :: m ()

  -- | Pop the solvers context-stack by one.
  pop :: m ()

  -- | Run check-sat on the current problem.
  checkSat :: m Result

  -- | Run get-model on the current problem.
  --   This can be used to decode temporary models within the SMT-Problem.
  --
  -- @
  -- x <- var @RealSort
  -- y <- var
  -- assert $ x >? y && y <? (-1)
  -- res <- checkSat
  -- case res of
  --   Unsat -> print "Unsat. Cannot get model."
  --   r     -> do
  --     model <- getModel
  --     liftIO $ print $ decode model x
  -- @
  getModel :: m Solution

  -- | Evaluate any expressions value in the solvers model.
  --   Requires a 'Sat' or 'Unknown' check-sat response beforehand.
  --
  -- @
  -- x <- var @RealSort
  -- assert $ x >? 10
  -- res <- checkSat
  -- case res of
  --   Unsat -> print "Unsat. Cannot get value for 'x'."
  --   r     -> do
  --     x' <- getValue x
  --     liftIO $ print $ show r ++ ": x = " ++ show x'
  -- @
  getValue :: KnownSMTSort t => Expr t -> m (Maybe (Decoded (Expr t)))

-- | First run 'checkSat' and then 'getModel' on the current problem.
solve :: (MonadIncrSMT s m, MonadIO m) => m (Result, Solution)
solve = liftM2 (,) checkSat getModel

-- | A 'MonadState' that holds an OMT-Problem.
--   An OMT-Problem is a 'SMT-Problem' with additional optimization targets.
class MonadSMT s m => MonadOMT s m where
  -- | Minimizes a numerical expression within the OMT-Problem.
  --
  --   For example, below minimization:
  --
  -- @
  -- x <- var @IntSort
  -- assert $ x >? -2
  -- minimize x
  -- @
  --
  --   will give @x := -1@ as solution.
  minimize :: (KnownSMTSort t, Num (Expr t)) => Expr t -> m ()

  -- | Maximizes a numerical expression within the OMT-Problem.
  --
  --   For example, below maximization:
  --
  -- @
  -- x <- var @(BvSort 8)
  -- maximize x
  -- @
  --
  --   will give @x := 11111111@ as solution.
  maximize :: (KnownSMTSort t, Num (Expr t)) => Expr t -> m ()

  -- | Asserts a soft boolean expression.
  --   May take a weight and an identifier for grouping.
  --
  --   For example, below a soft constraint with weight 2.0 and identifier \"myId\" for grouping:
  --
  -- @
  -- x <- var @BoolSort
  -- assertSoft x (Just 2.0) (Just "myId")
  -- @
  --
  --   Omitting the weight will default it to 1.0.
  --
  -- @
  -- x <- var @BoolSort
  -- y <- var @BoolSort
  -- assertSoft x
  -- assertSoft y (Just "myId")
  -- @
  assertSoft :: Expr BoolSort -> Maybe Double -> Maybe String -> m ()

-- | Like 'assertSoft' but forces a weight and omits the group-id.
assertSoftWeighted :: MonadOMT s m => Expr BoolSort -> Double -> m ()
assertSoftWeighted expr w = assertSoft expr (Just w) Nothing
