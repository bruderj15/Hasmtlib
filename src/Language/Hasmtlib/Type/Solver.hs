{-# LANGUAGE TemplateHaskell #-}

{- |
This module provides functions for having SMT-Problems solved by external solvers.
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

  -- ** Debugger
  , Debugger, StateDebugger(..), PipeDebugger(..)

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
import Language.Hasmtlib.Type.Expr
import Language.Hasmtlib.Type.SMTSort
import Language.Hasmtlib.Type.Solution
import Language.Hasmtlib.Type.SMT
import Language.Hasmtlib.Type.OMT
import Language.Hasmtlib.Type.Pipe
import Language.Hasmtlib.Codec
import Data.Sequence as Seq hiding ((|>), filter)
import Data.ByteString.Lazy hiding (singleton)
import Data.ByteString.Lazy.UTF8 (toString)
import Data.ByteString.Builder
import qualified Data.ByteString.Lazy.Char8 as ByteString.Char8
import qualified SMTLIB.Backends as Backend
import qualified SMTLIB.Backends.Process as Process
import Data.Default
import Data.Maybe
import Data.Kind
import Data.Attoparsec.ByteString (parseOnly)
import Control.Lens hiding (op)
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans.Control
import System.Timeout.Lifted

-- | Function that turns a state into a 'Result' and a 'Solution'.
type Solver s m = s -> m (Result, Solution)

-- | Computes a debugger from a state.
type family Debugger s :: Type
type instance Debugger SMT = StateDebugger SMT
type instance Debugger OMT = StateDebugger OMT
type instance Debugger Pipe = PipeDebugger

-- | A type holding actions for debugging states holding SMT-Problems.
data StateDebugger s = StateDebugger
  { debugState          :: s -> IO ()               -- ^ Debug the entire state
  , debugProblem        :: Seq Builder -> IO ()     -- ^ Debug the linewise-rendered problem
  , debugResultResponse :: ByteString -> IO ()      -- ^ Debug the solvers raw response for @(check-sat)@
  , debugModelResponse  :: ByteString -> IO ()      -- ^ Debug the solvers raw response for @(get-model)@
  }

instance Default (StateDebugger SMT) where
  def = StateDebugger
    { debugState            = \s -> liftIO $ do
        putStrLn $ "Vars: "       ++ show (Seq.length (s^.vars))
        putStrLn $ "Assertions: " ++ show (Seq.length (s^.formulas))
    , debugProblem        = liftIO . mapM_ (ByteString.Char8.putStrLn . toLazyByteString)
    , debugResultResponse = liftIO . putStrLn . (\s -> "\n" ++ s ++ "\n") . toString
    , debugModelResponse  = liftIO . mapM_ ByteString.Char8.putStrLn . split 13
    }

instance Default (StateDebugger OMT) where
  def = StateDebugger
    { debugState          = \omt -> liftIO $ do
        putStrLn $ "Vars: "                 ++ show (Seq.length (omt^.smt.vars))
        putStrLn $ "Hard assertions: "      ++ show (Seq.length (omt^.smt.formulas))
        putStrLn $ "Soft assertions: "      ++ show (Seq.length (omt^.softFormulas))
        putStrLn $ "Optimization targets: " ++ show (Seq.length (omt^.targetMinimize) + Seq.length (omt^.targetMaximize))
    , debugProblem        = liftIO . mapM_ (ByteString.Char8.putStrLn . toLazyByteString)
    , debugResultResponse = liftIO . putStrLn . (\s -> "\n" ++ s ++ "\n") . toString
    , debugModelResponse  = liftIO . mapM_ ByteString.Char8.putStrLn . split 13
    }

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
solver :: (RenderSeq s, MonadIO m, Debugger s ~ StateDebugger s) => SolverConfig s -> Solver s m
solver (SolverConfig cfg mTO debugger) s = do
  liftIO $ Process.with cfg $ \handle -> do
    maybe mempty (`debugState` s) debugger
    pSolver <- Backend.initSolver Backend.Queuing $ Process.toBackend handle

    let problem = renderSeq s
    maybe mempty (`debugProblem` problem) debugger
    forM_ problem (Backend.command_ pSolver)

    let timingOut io = case mTO of
          Nothing -> io
          Just t -> fromMaybe "unknown" <$> timeout t io
    resultResponse <- timingOut $ Backend.command pSolver "(check-sat)"
    maybe mempty (`debugResultResponse` resultResponse) debugger

    modelResponse <- Backend.command pSolver "(get-model)"
    maybe mempty (`debugModelResponse` modelResponse) debugger

    case parseOnly resultParser (toStrict resultResponse) of
      Left e    -> fail e
      Right res -> case res of
        Unsat -> return (res, mempty)
        _     -> case parseOnly anyModelParser (toStrict modelResponse) of
          Left e    -> fail e
          Right sol -> return (res, sol)

-- | Decorates a 'SolverConfig' with a timeout. The timeout is given as an 'Int' which specifies
--   after how many __microseconds__ the external solver process shall time out on @(check-sat)@.
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
--   interactiveWith (solver z3) $ do
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
