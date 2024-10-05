{-# LANGUAGE TemplateHaskell #-}

{- |
This module provides functions for having SMT-Problems solved by external solvers.

The base type for every solver is the 'SolverConfig'. It describes where the solver executable is located and how it should behave.
You can provide a time-out using the decorator 'timingout'.
Another decorator - 'debugging' - allows you to debug all the information you want. The actual debuggers can be found in "Language.Hasmtlib.Type.Debugger".
-}
module Language.Hasmtlib.Type.Solver
  (
  -- * Solver configuration

  -- ** Type
  SolverConfig(..)

  -- ** Decoration
  , debugging, timingout

  -- ** Lens
  , processConfig, mTimeout, mDebugger

  -- * Stateful solving
  , Solver, solver, solveWith

  -- * Interactive solving
  , interactiveWith

  -- ** Minimzation
  , solveMinimized

  -- ** Maximization
  , solveMaximized
  )
where

import Language.Hasmtlib.Internal.Render
import Language.Hasmtlib.Internal.Parser
import Language.Hasmtlib.Type.MonadSMT
import Language.Hasmtlib.Type.Debugger
import Language.Hasmtlib.Type.Expr
import Language.Hasmtlib.Type.SMTSort
import Language.Hasmtlib.Type.Solution
import Language.Hasmtlib.Type.Pipe
import Language.Hasmtlib.Codec
import Data.ByteString.Lazy hiding (singleton)
import qualified SMTLIB.Backends as Backend
import qualified SMTLIB.Backends.Process as Process
import Data.Default
import Data.Maybe
import Data.Attoparsec.ByteString (parseOnly)
import Control.Lens hiding (op)
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans.Control
import System.Timeout.Lifted

-- | Function that turns a state into a 'Result' and a 'Solution'.
type Solver s m = s -> m (Result, Solution)

-- | Configuration for solver processes.
data SolverConfig s = SolverConfig
  { _processConfig  :: Process.Config         -- ^ The underlying config of the process
  , _mTimeout       :: Maybe Int              -- ^ Timeout in microseconds
  , _mDebugger      :: Maybe (Debugger s)     -- ^ Debugger for communication with external solver
  }
$(makeLenses ''SolverConfig)

-- | Creates a 'Solver' which holds an external process with a SMT-Solver.
--
--   This will:
--
-- 1. Encode the SMT-problem,
--
-- 2. start a new external process for the SMT-Solver,
--
-- 3. send the problem to the SMT-Solver,
--
-- 4. wait for an answer and parse it,
--
-- 5. close the process and clean up all resources and
--
-- 6. return the decoded solution.
solver :: (RenderProblem s, MonadIO m) => SolverConfig s -> Solver s m
solver (SolverConfig cfg mTO debugger) s = do
  handle <- liftIO $ Process.new cfg
  let timingOut io = case mTO of
        Nothing -> io
        Just t -> fromMaybe ("unknown", mempty) <$> timeout t io
  (rawRes, rawModel) <- liftIO $ timingOut $ do
    maybe mempty (`debugState` s) debugger
    pSolver <- Backend.initSolver Backend.Queuing $ Process.toBackend handle

    let os   = renderOptions s
        l    = renderLogic s
        vs   = renderDeclareVars s
        as   = renderAssertions s
        sas  = renderSoftAssertions s
        mins = renderMinimizations s
        maxs = renderMaximizations s
    maybe mempty (forM_ os . debugOption) debugger
    forM_ os (Backend.command_ pSolver)
    maybe mempty (`debugLogic` l) debugger
    Backend.command_ pSolver l
    maybe mempty (forM_ vs . debugVar) debugger
    forM_ vs (Backend.command_ pSolver)
    maybe mempty (forM_ as . debugAssert) debugger
    forM_ as (Backend.command_ pSolver)
    maybe mempty (forM_ sas . debugAssertSoft) debugger
    forM_ sas (Backend.command_ pSolver)
    maybe mempty (forM_ mins . debugMinimize) debugger
    forM_ mins (Backend.command_ pSolver)
    maybe mempty (forM_ maxs . debugMaximize) debugger
    forM_ maxs (Backend.command_ pSolver)

    maybe mempty (`debugCheckSat` "(check-sat)") debugger
    resultResponse <- Backend.command pSolver renderCheckSat
    maybe mempty (`debugResultResponse` resultResponse) debugger

    maybe mempty (`debugGetModel` "(get-model)") debugger
    modelResponse <- Backend.command pSolver renderGetModel
    maybe mempty (`debugModelResponse` modelResponse) debugger

    return (resultResponse, modelResponse)

  liftIO $ Process.close handle

  case parseOnly resultParser (toStrict rawRes) of
    Left e    -> error $ "Language.Hasmtlib.Type.Solver#solver: Error when paring (check-sat) response: "
                        <> e <> " | This is probably an error in Hasmtlib."
    Right res -> case res of
        Unsat -> return (res, mempty)
        _     -> case parseOnly anyModelParser (toStrict rawModel) of
          Left e    -> error $ "Language.Hasmtlib.Type.Solver#solver: Error when paring (get-model) response: "
                              <> e <> " | This is probably an error in Hasmtlib."
          Right sol -> return (res, sol)

-- | Decorates a 'SolverConfig' with a timeout. The timeout is given as an 'Int' which specifies
--   after how many __microseconds__ the entire problem including problem construction,
--   solver interaction and solving time may time out.
--
--   When timing out, the 'Result' will always be 'Unknown'.
--
--   This uses 'timeout' internally.
timingout :: Int -> SolverConfig s -> SolverConfig s
timingout t cfg = cfg & mTimeout ?~ t

-- | Decorates a 'SolverConfig' with a 'Debugger'.
debugging :: Debugger s -> SolverConfig s -> SolverConfig s
debugging debugger cfg = cfg & mDebugger ?~ debugger

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
solveWith solving m = do
  (a, problem) <- runStateT m def
  (result, solution) <- solving problem

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
--   interactiveWith z3 $ do
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
interactiveWith :: (MonadIO m, MonadBaseControl IO m) => SolverConfig Pipe -> StateT Pipe m a -> m (Maybe a)
interactiveWith cfg m = do
  handle <- liftIO $ Process.new $ cfg^.processConfig
  processSolver <- liftIO $ Backend.initSolver Backend.Queuing $ Process.toBackend handle
  let timingOut io = case cfg^.mTimeout of
        Nothing -> Just <$> io
        Just t -> timeout t io
  ma <- timingOut $ runStateT m $ Pipe 0 Nothing def mempty mempty processSolver (cfg^.mDebugger)
  liftIO $ Process.close handle
  return $ fmap fst ma

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
solveOptimized op goal mStep mDebug = refine Unsat mempty goal 0
  where
    refine oldRes oldSol target n_pushes = do
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
              refine res sol target (n_pushes + 1)
        r -> do
          if n_pushes < 1
          then return (r, mempty)
          else case mStep of
            Nothing -> do
              replicateM_ n_pushes pop
              return (oldRes, oldSol)
            Just _ -> do
              pop
              opt <- solveOptimized op goal Nothing mDebug -- make sure the very last step did not skip the optimal result
              replicateM_ (n_pushes - 1) pop
              return opt
