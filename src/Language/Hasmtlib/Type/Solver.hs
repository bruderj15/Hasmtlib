{- |
This module provides functions for having SMT-Problems solved by external solvers.
-}
module Language.Hasmtlib.Type.Solver
  (
  -- * WithSolver
  WithSolver(..)

  -- * Stateful solving
  , solveWith

  -- * Interactive solving
  , interactiveWith, debugInteractiveWith

  -- ** Minimzation
  , solveMinimized

  -- ** Maximization
  , solveMaximized
  )
where

import Language.Hasmtlib.Type.MonadSMT
import Language.Hasmtlib.Type.Expr
import Language.Hasmtlib.Type.SMTSort
import Language.Hasmtlib.Type.Solution
import Language.Hasmtlib.Type.Pipe
import Language.Hasmtlib.Codec
import qualified SMTLIB.Backends as Backend
import qualified SMTLIB.Backends.Process as Process
import Data.Default
import Data.Maybe
import Control.Monad.State

-- | Data that can have a 'Backend.Solver' which may be debugged.
class WithSolver a where
  -- | Create a value with a 'Backend.Solver' and a 'Bool' for whether to debug the 'Backend.Solver'.
  withSolver :: Backend.Solver -> Bool -> a

instance WithSolver Pipe where
  withSolver = Pipe 0 Nothing def mempty mempty

-- | @'solveWith' solver prob@ solves a SMT problem @prob@ with the given
-- @solver@. It returns a pair consisting of:
--
-- 1. A 'Result' that indicates if @prob@ is satisfiable ('Sat'),
--    unsatisfiable ('Unsat'), or if the solver could not determine any
--    results ('Unknown').
--
-- 2. A 'Decoded' answer that was decoded using the solution to @prob@. Note
--    that this answer is only meaningful if the 'Result' is 'Sat' or 'Unknown' and
--    the answer value is in a 'Just'.
--
-- ==== __Example__
--
-- @
-- import Language.Hasmtlib
--
-- main :: IO ()
-- main = do
--   res <- solveWith @SMT (solver cvc5) $ do
--     setLogic \"QF_LIA\"
--
--     x <- var \@IntSort
--
--     assert $ x >? 0
--
--     return x
--
--   print res
-- @
--
-- The solver will probably answer with @x := 1@.
solveWith :: (Default s, Monad m, Codec a) => Solver s m -> StateT s m a -> m (Result, Maybe (Decoded a))
solveWith solver m = do
  (a, problem) <- runStateT m def
  (result, solution) <- solver problem

  return (result, decode solution a)

-- | Pipes an SMT-problem interactively to the solver.
--
-- ==== __Example__
--
-- @
-- import Language.Hasmtlib
-- import Control.Monad.IO.Class
--
-- main :: IO ()
-- main = do
--   cvc5Living <- interactiveSolver cvc5
--   interactiveWith @Pipe cvc5Living $ do
--     setOption $ Incremental True
--     setOption $ ProduceModels True
--     setLogic \"QF_LRA\"
--
--     x <- var \@RealSort
--
--     assert $ x >? 0
--
--     (res, sol) <- solve
--     liftIO $ print res
--     liftIO $ print $ decode sol x
--
--     push
--     y <- var \@IntSort
--
--     assert $ y <? 0
--     assert $ x === y
--
--     res' <- checkSat
--     liftIO $ print res'
--     pop
--
--     res'' <- checkSat
--     liftIO $ print res''
--
--   return ()
-- @
interactiveWith :: (WithSolver s, MonadIO m) => (Backend.Solver, Process.Handle) -> StateT s m () -> m ()
interactiveWith (solver, handle) m = do
  _ <- runStateT m $ withSolver solver False
  liftIO $ Process.close handle

-- | Like 'interactiveWith' but it prints all communication with the solver to console.
debugInteractiveWith :: (WithSolver s, MonadIO m) => (Backend.Solver, Process.Handle) -> StateT s m () -> m ()
debugInteractiveWith (solver, handle) m = do
  _ <- runStateT m $ withSolver solver True
  liftIO $ Process.close handle

-- | Solves the current problem with respect to a minimal solution for a given numerical expression.
--
--   This is done by incrementally refining the upper bound for a given target.
--   Terminates, when setting the last intermediate result as new upper bound results in 'Unsat'.
--   Then removes that last assertion and returns the previous (now confirmed minimal) result.
--
--   You can also provide a step-size. You do not have to worry about stepping over the optimal result.
--   This implementation takes care of it.
--
--   Access to intermediate results is also possible via an 'IO'-Action.
--
-- ==== __Examples__
--
-- @
-- x <- var \@IntSort
-- assert $ x >? 4
-- solveMinimized x Nothing Nothing
-- @
--
-- The solver will return @x := 5@.
--
-- The first 'Nothing' indicates that each intermediate result will be set as next upper bound.
-- The second 'Nothing' shows that we do not care about intermediate, but only the final (minimal) result.
--
-- @
-- x <- var \@IntSort
-- assert $ x >? 4
-- solveMinimized x (Just (\\r -> r-1)) (Just print)
-- @
--
-- The solver will still return @x := 5@.
--
-- However, here we want the next bound of each refinement to be @lastResult - 1@.
-- Also, every intermediate result is printed.
solveMinimized :: (MonadIncrSMT Pipe m, MonadIO m, KnownSMTSort t, Orderable (Expr t))
  => Expr t                             -- ^ Target to minimize
  -> Maybe (Expr t -> Expr t)           -- ^ Step-size: Lambda is given last result as argument, producing the next upper bound
  -> Maybe (Solution -> IO ())          -- ^ Accessor to intermediate results
  -> m (Result, Solution)
solveMinimized = solveOptimized (<?)

-- | Solves the current problem with respect to a maximal solution for a given numerical expression.
--
--   This is done by incrementally refining the lower bound for a given target.
--   Terminates, when setting the last intermediate result as new lower bound results in 'Unsat'.
--   Then removes that last assertion and returns the previous (now confirmed maximal) result.
--
--   You can also provide a step-size. You do not have to worry about stepping over the optimal result.
--   This implementation takes care of it.
--
--   Access to intermediate results is also possible via an 'IO'-Action.
--
-- ==== __Examples__
--
-- @
-- x <- var \@IntSort
-- assert $ x <? 4
-- solveMaximized x Nothing Nothing
-- @
--
-- The solver will return @x := 3@.
--
-- The first 'Nothing' indicates that each intermediate result will be set as next lower bound.
-- The second 'Nothing' shows that we do not care about intermediate, but only the final (maximal) result.
--
-- @
-- x <- var \@IntSort
-- assert $ x <? 4
-- solveMinimized x (Just (+1)) (Just print)
-- @
--
-- The solver will still return @x := 3@.
--
-- However, here we want the next bound of each refinement to be @lastResult + 1@.
-- Also, every intermediate result is printed.
solveMaximized :: (MonadIncrSMT Pipe m, MonadIO m, KnownSMTSort t, Orderable (Expr t))
  => Expr t                             -- ^ Target to maximize
  -> Maybe (Expr t -> Expr t)           -- ^ Step-size: Lambda is given last result as argument, producing the next lower bound
  -> Maybe (Solution -> IO ())          -- ^ Accessor to intermediate results
  -> m (Result, Solution)
solveMaximized = solveOptimized (>?)

solveOptimized :: (MonadIncrSMT Pipe m, MonadIO m, KnownSMTSort t)
  => (Expr t -> Expr t -> Expr BoolSort)
  -> Expr t
  -> Maybe (Expr t -> Expr t)
  -> Maybe (Solution -> IO ())
  -> m (Result, Solution)
solveOptimized op goal mStep mDebug = refine Unknown mempty goal
  where
    refine oldRes oldSol target = do
      res <- checkSat
      case res of
        Sat   -> do
          sol <- getModel
          case decode sol target of
            Nothing        -> return (Sat, mempty)
            Just targetSol -> do
              case mDebug of
                Nothing    -> pure ()
                Just debug -> liftIO $ debug sol
              push
              let step = fromMaybe id mStep
              assert $ target `op` step (encode targetSol)
              refine res sol target
        _ -> do
          pop
          case mStep of
            Nothing -> return (oldRes, oldSol)
            Just _  -> solveOptimized op goal Nothing mDebug -- make sure the very last step did not skip the optimal result
