{-# LANGUAGE LambdaCase #-}

{- |
This module provides MTL-Style-classes for building SMT-problems.

The following three classes form the core of this module:

1. 'MonadSMT' for plain SMT-problems. Create variables using 'var' and assert formulas via 'assert'.

2. 'MonadIncrSMT' for plain SMT-problems with addtional access to the external solvers incremental stack and it's operations.

3. 'MonadOMT' for SMT-problems with optimization. Optimize via 'minimize' and 'maximize' and softly assert formulas via 'assertSoft'.
-}
module Language.Hasmtlib.Type.MonadSMT
(
  -- * MonadSMT
  MonadSMT(..)
, var, smtvar
, constant, assertMaybe, quantify

  -- * MonadIncrSMT
, MonadIncrSMT(..)
, solve

  -- * MonadOMT
, MonadOMT(..)
, assertSoftWeighted
)
where

import Language.Hasmtlib.Type.Expr
import Language.Hasmtlib.Type.Value
import Language.Hasmtlib.Type.Option
import Language.Hasmtlib.Type.SMTSort
import Language.Hasmtlib.Type.Solution
import Language.Hasmtlib.Codec
import Data.Proxy
import Control.Lens
import Control.Monad
import Control.Monad.State

-- | A 'MonadState' that holds an SMT-Problem.
--
-- ==== __Example__
--
-- @
-- problem :: MonadSMT s m => StateT s m (Expr IntSort)
-- problem = do
--   setLogic \"QF_LIA\"
--   x <- var @IntSort
--   assert $ x + 2 === x * 2
--   return x
-- @
class MonadState s m => MonadSMT s m where
  -- | Construct a variable.
  --   This is mainly intended for internal use.
  --   In the API use 'var'' instead.
  smtvar' :: forall t. KnownSMTSort t => Proxy t -> m (SMTVar t)

  -- | Construct a variable as expression.
  --
  -- ==== __Example__
  --
  -- @
  -- x <- var' (Proxy @RealType)
  -- @
  var' :: forall t. KnownSMTSort t => Proxy t -> m (Expr t)

  -- | Assert a boolean expression.
  --
  -- ==== __Example__
  --
  -- @
  -- x <- var @IntType
  -- assert $ x - 27 === 42
  -- @
  assert :: Expr BoolSort -> m ()

  -- | Set an SMT-Solver-Option.
  --
  -- ==== __Example__
  --
  -- @
  -- setOption $ Incremental True
  -- @
  setOption :: SMTOption -> m ()

  -- | Set the logic for the SMT-Solver to use.
  --
  -- ==== __Example__
  --
  -- @
  -- setLogic \"QF_LRA\"
  -- @
  setLogic :: String -> m ()

-- | Wrapper for 'var'' which hides the 'Proxy'.
--
-- ==== __Example__
--
-- @
-- x <- var @BoolSort
-- @
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
-- ==== __Examples__
--
--   >>> constant True
--       Constant (BoolValue True)
--
--   >>> constant (10 :: Integer)
--       Constant (IntValue 10)
--
--   >>> constant @RealSort 5
--       Constant (RealValue 5.0)
--
--   >>> constant @(BvSort Unsigned 8) 14
--       Constant (BvValue 00001110)
constant :: KnownSMTSort t => HaskellType t -> Expr t
constant = Constant . wrapValue
{-# INLINE constant #-}

-- | Maybe assert a boolean expression.
--
--   Asserts given expression if 'Maybe' is a 'Just'.
--   Does nothing otherwise.
assertMaybe :: MonadSMT s m => Maybe (Expr BoolSort) -> m ()
assertMaybe Nothing = return ()
assertMaybe (Just expr) = assert expr

-- | Assign quantified variables to all quantified subexpressions of an expression.
--
--   Quantifies bottom-up.
--
--   This is intended for internal use.
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

-- | A 'MonadSMT' that addtionally allows incremental solving with access to a solvers incremental stack.
--
-- Some solvers require to have 'SMTOption' 'Incremental' set first.
--
-- ==== __Example__
--
-- @
-- problem :: MonadIncrSMT s m => StateT s m ()
-- problem = do
--   setOption $ Incremental True
--   setLogic \"QF_LIA\"
--   x <- var @IntSort
--   push
--   assert $ x + 2 === x * 2
--   res <- checkSat
--   case res of
--     Sat -> do
--       x' <- getValue x
--       ...
--     _ -> pop ...
--   return ()
-- @
class MonadSMT s m => MonadIncrSMT s m where
  -- | Push a new context (one) to the solvers context-stack.
  push :: m ()

  -- | Pop the solvers context-stack by one.
  pop :: m ()

  -- | Run @check-sat@ on the current problem.
  checkSat :: m Result

  -- | Run get-model on the current problem.
  --   This can be used to decode temporary models within the SMT-Problem.
  --
  -- ==== __Example__
  --
  -- @
  -- x <- var @RealSort
  -- y <- var
  -- assert $ x >? y && y <? (-1)
  -- res <- checkSat
  -- case res of
  --   Sat -> do
  --     model <- getModel
  --     liftIO $ print $ decode model x
  --   r -> print $ show r <> ": Cannot get model."
  -- @
  getModel :: m Solution

  -- | Evaluate any expressions value in the solvers model.
  --   Requires a 'Sat' or 'Unknown' check-sat response beforehand.
  --
  -- ==== __Example__
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
--
-- ==== __Example__
--
-- @
-- x <- var @BoolSort
-- assert $ x `xor` false
-- (res, sol) <- solve
-- case res of
--   Sat -> do
--     x' <- getValue x
--     liftIO $ print $ decode sol x
--   r -> print r
-- @
solve :: (MonadIncrSMT s m, MonadIO m) => m (Result, Solution)
solve = liftM2 (,) checkSat getModel

-- | A 'MonadSMT' that addtionally allows optimization targets.
--
-- ==== __Example__
--
-- @
-- problem :: MonadOMT s m => StateT s m (Expr (BvSort Unsigned 8))
-- problem = do
--   setLogic \"QF_BV\"
--   x <- var @(BvSort Unsigned 8)
--   assertSoftWeighted (x <? maxBound) 2.0
--   maximize x
--   return x
-- @
class MonadSMT s m => MonadOMT s m where
  -- | Minimizes a numerical expression within the OMT-Problem.
  --
  -- ==== __Example__
  --
  -- @
  -- x <- var @IntSort
  -- assert $ x >? -2
  -- minimize x
  -- @
  --
  -- will give @x := -1@ as solution.
  minimize :: (KnownSMTSort t, Num (Expr t)) => Expr t -> m ()

  -- | Maximizes a numerical expression within the OMT-Problem.
  --
  -- ==== __Example__
  --
  -- @
  -- x <- var @(BvSort Signed 4)
  -- assert $ x <? 2
  -- maximize x
  -- @
  --
  -- will give @x := 0001@ as solution.
  maximize :: (KnownSMTSort t, Num (Expr t)) => Expr t -> m ()

  -- | Softly asserts a boolean expression.
  --
  --   May take a weight and an identifier for grouping.
  --
  -- ==== __Example__
  --
  -- @
  -- x <- var @BoolSort
  -- assertSoft x (Just 0.5) (Just "myId")
  -- @
  assertSoft :: Expr BoolSort -> Maybe Double -> Maybe String -> m ()

-- | Like 'assertSoft' but forces a weight and omits the group-id.
assertSoftWeighted :: MonadOMT s m => Expr BoolSort -> Double -> m ()
assertSoftWeighted expr w = assertSoft expr (Just w) Nothing
