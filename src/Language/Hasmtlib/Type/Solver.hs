module Language.Hasmtlib.Type.Solver
  ( WithSolver(..)
  , solveWith
  , interactiveWith, debugInteractiveWith
  , solveMinimized, solveMinimizedDebug
  , solveMaximized, solveMaximizedDebug
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
import Control.Monad.State

-- | Data that can have a 'Backend.Solver' which may be debugged.
class WithSolver a where
  -- | Create a datum with a 'Backend.Solver' and a 'Bool for whether to debug the 'Backend.Solver'.
  withSolver :: Backend.Solver -> Bool -> a

instance WithSolver Pipe where
  withSolver = Pipe 0 Nothing 0 mempty mempty

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
-- Here is a small example of how to use 'solveWith':
--
-- @
-- import Language.Hasmtlib
--
-- main :: IO ()
-- main = do
--   res <- solveWith @SMT (solver cvc5) $ do
--     setLogic \"QF_LIA\"
--
--     x <- var @IntSort
--
--     assert $ x >? 0
--
--     return x
--
--   print res
-- @
solveWith :: (Default s, Monad m, Codec a) => Solver s m -> StateT s m a -> m (Result, Maybe (Decoded a))
solveWith solver m = do
  (a, problem) <- runStateT m def
  (result, solution) <- solver problem

  return (result, decode solution a)

-- | Pipes an SMT-problem interactively to the solver.
--   Enables incremental solving by default.
--   Here is a small example of how to use it for solving a problem utilizing the solvers incremental stack:
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
--     setLogic \"QF_LIA\"
--
--     x <- var @IntSort
--
--     assert $ x >? 0
--
--     (res, sol) <- solve
--     liftIO $ print res
--     liftIO $ print $ decode sol x
--
--     push
--     y <- var @IntSort
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
--   Does not rely on MaxSMT/OMT.
--   Instead uses iterative refinement.
--
--   If you want access to intermediate results, use 'solveMinimizedDebug' instead.
solveMinimized :: (MonadIncrSMT Pipe m, MonadIO m, KnownSMTSort t, Orderable (Expr t))
  => Expr t
  -> m (Result, Solution)
solveMinimized = solveOptimized Nothing (<?)

-- | Like 'solveMinimized' but with access to intermediate results.
solveMinimizedDebug :: (MonadIncrSMT Pipe m, MonadIO m, KnownSMTSort t, Orderable (Expr t))
  => (Solution -> IO ())
  -> Expr t
  -> m (Result, Solution)
solveMinimizedDebug debug = solveOptimized (Just debug) (<?)

-- | Solves the current problem with respect to a maximal solution for a given numerical expression.
--
--   Does not rely on MaxSMT/OMT.
--   Instead uses iterative refinement.
--
--   If you want access to intermediate results, use 'solveMaximizedDebug' instead.
solveMaximized :: (MonadIncrSMT Pipe m, MonadIO m, KnownSMTSort t, Orderable (Expr t))
  => Expr t
  -> m (Result, Solution)
solveMaximized = solveOptimized Nothing (>?)

-- | Like 'solveMaximized' but with access to intermediate results.
solveMaximizedDebug :: (MonadIncrSMT Pipe m, MonadIO m, KnownSMTSort t, Orderable (Expr t))
  => (Solution -> IO ())
  -> Expr t
  -> m (Result, Solution)
solveMaximizedDebug debug = solveOptimized (Just debug) (>?)

solveOptimized :: (MonadIncrSMT Pipe m, MonadIO m, KnownSMTSort t)
  => Maybe (Solution -> IO ())
  -> (Expr t -> Expr t -> Expr BoolSort)
  -> Expr t
  -> m (Result, Solution)
solveOptimized mDebug op = go Unknown mempty
  where
    go oldRes oldSol target = do
      push
      (res, sol) <- solve
      case res of
        Sat   -> do
          case decode sol target of
            Nothing        -> return (Sat, mempty)
            Just targetSol -> do
              case mDebug of
                Nothing    -> pure ()
                Just debug -> liftIO $ debug sol
              assert $ target `op` encode targetSol
              go res sol target
        _ -> pop >> return (oldRes, oldSol)
