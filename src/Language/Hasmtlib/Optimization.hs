-- we want to restrict minimize and maximize to numerical expressions, therefore redundant Num (Expr t)
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Language.Hasmtlib.Optimization
  ( minimize, maximize
  , solveMinimized, solveMaximized
  , solveMinimizedDebug, solveMaximizedDebug
  )
where

import Language.Hasmtlib.Codec
import Language.Hasmtlib.Orderable
import Language.Hasmtlib.Type.Pipe
import Language.Hasmtlib.Type.MonadSMT
import Language.Hasmtlib.Type.Solution
import Language.Hasmtlib.Internal.Expr
import Language.Hasmtlib.Internal.Render
import qualified SMTLIB.Backends as B
import Control.Monad.IO.Class
import Control.Monad.State
import Control.Lens hiding (op)

-- | Minimize a numerical expression within the SMT-Problem.
--   This is a MaxSMT/OMT operation.
--
--   __Caution:__ Currently we do not restrict usage of this function to solvers who actually support it.
--   When using 'minimize', make sure your solver supports MaxSMT/OMT.
minimize :: (MonadIncrSMT Pipe m, MonadIO m, KnownSMTSort t, Num (Expr t)) => Expr t -> m ()
minimize expr = do
  smt <- get
  liftIO $ B.command_ (smt^.pipe) $ "(minimize " <> render expr <> ")"

-- | Maximize a numerical expression within the SMT-Problem.
--   This is a MaxSMT/OMT operation.
--
--   __Caution:__ Currently we do not restrict usage of this function to solvers who actually support it.
--   When using 'maximize', make sure your solver supports MaxSMT/OMT.
maximize :: (MonadIncrSMT Pipe m, MonadIO m, KnownSMTSort t, Num (Expr t)) => Expr t -> m ()
maximize expr = do
  smt <- get
  liftIO $ B.command_ (smt^.pipe) $ "(maximize " <> render expr <> ")"

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
solveMaximizedDebug debug = solveOptimized (Just debug) (<?)

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
