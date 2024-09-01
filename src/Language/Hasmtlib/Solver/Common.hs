{- |
This module handles common IO interaction with external SMT-Solvers via external processes.

It is built on top of Tweag's package @smtlib-backends@.

Although there already are several concrete solvers like @Z3@ in @Language.Hasmtlib.Solver.Z3@,
you may use this module to create your own solver bindings.

Just supply a 'Process.Config' for function 'solver'.
-}
module Language.Hasmtlib.Solver.Common
(
  -- * Construction
  processSolver
, solver
, interactiveSolver

  -- * Debugging
, Debugger(..)
, debug
, def
)
where

import Language.Hasmtlib.Type.SMT
import Language.Hasmtlib.Type.OMT
import Language.Hasmtlib.Type.Solution
import Language.Hasmtlib.Internal.Render
import Language.Hasmtlib.Internal.Parser
import Data.Default
import Data.Sequence as Seq hiding ((|>), filter)
import Data.ByteString.Lazy hiding (singleton)
import Data.ByteString.Lazy.UTF8 (toString)
import Data.ByteString.Builder
import Data.Attoparsec.ByteString
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import qualified SMTLIB.Backends.Process as Process
import qualified SMTLIB.Backends as Backend

-- | Creates a 'Solver' from a 'Process.Config'.
solver :: (RenderSeq s, MonadIO m) => Process.Config -> Solver s m
solver cfg = processSolver cfg Nothing

-- | Creates a debugging 'Solver' from a 'Process.Config'.
debug :: (RenderSeq s, MonadIO m) => Process.Config -> Debugger s -> Solver s m
debug cfg = processSolver cfg . Just

-- | Creates an interactive session with a solver by creating and returning an alive process-handle 'Process.Handle'.
--   Queues commands by default, see 'Backend.Queuing'.
interactiveSolver :: MonadIO m => Process.Config -> m (Backend.Solver, Process.Handle)
interactiveSolver cfg = liftIO $ do
  handle  <- Process.new cfg
  liftM2 (,) (Backend.initSolver Backend.Queuing $ Process.toBackend handle) (return handle)

-- | A type holding actions for debugging states.
data Debugger s = Debugger
  { debugState          :: s -> IO ()               -- ^ Debug the entire state
  , debugProblem        :: Seq Builder -> IO ()     -- ^ Debug the linewise-rendered problem
  , debugResultResponse :: ByteString -> IO ()      -- ^ Debug the solvers raw response for @(check-sat)@
  , debugModelResponse  :: ByteString -> IO ()      -- ^ Debug the solvers raw response for @(get-model)@
  }

instance Default (Debugger SMT) where
  def = Debugger
    { debugState            = \s -> liftIO $ do
        putStrLn $ "Vars: "       ++ show (Seq.length (s^.vars))
        putStrLn $ "Assertions: " ++ show (Seq.length (s^.formulas))
    , debugProblem        = liftIO . mapM_ (putStrLn . toString . toLazyByteString)
    , debugResultResponse = liftIO . putStrLn . (\s -> "\n" ++ s ++ "\n") . toString
    , debugModelResponse  = liftIO . mapM_ (putStrLn . toString) . split 13
    }

instance Default (Debugger OMT) where
  def = Debugger
    { debugState          = \omt -> liftIO $ do
        putStrLn $ "Vars: "                 ++ show (Seq.length (omt^.smt.vars))
        putStrLn $ "Hard assertions: "      ++ show (Seq.length (omt^.smt.formulas))
        putStrLn $ "Soft assertions: "      ++ show (Seq.length (omt^.softFormulas))
        putStrLn $ "Optimization targets: " ++ show (Seq.length (omt^.targetMinimize) + Seq.length (omt^.targetMaximize))
    , debugProblem        = liftIO . mapM_ (putStrLn . toString . toLazyByteString)
    , debugResultResponse = liftIO . putStrLn . (\s -> "\n" ++ s ++ "\n") . toString
    , debugModelResponse  = liftIO . mapM_ (putStrLn . toString) . split 13
    }

-- | A 'Solver' which holds an external process with a SMT-Solver.
--   This will:
--
-- 1. Encode the 'SMT'-problem,
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
processSolver :: (RenderSeq s, MonadIO m) => Process.Config -> Maybe (Debugger s) -> Solver s m
processSolver cfg debugger s = do
  liftIO $ Process.with cfg $ \handle -> do
    maybe mempty (`debugState` s) debugger
    pSolver <- Backend.initSolver Backend.Queuing $ Process.toBackend handle

    let problem = renderSeq s
    maybe mempty (`debugProblem` problem) debugger

    forM_ problem (Backend.command_ pSolver)
    resultResponse <- Backend.command pSolver "(check-sat)"
    maybe mempty (`debugResultResponse` resultResponse) debugger

    modelResponse  <- Backend.command pSolver "(get-model)"
    maybe mempty (`debugModelResponse` modelResponse) debugger

    case parseOnly resultParser (toStrict resultResponse) of
      Left e    -> fail e
      Right res -> case res of
        Unsat -> return (res, mempty)
        _     -> case parseOnly anyModelParser (toStrict modelResponse) of
          Left e    -> fail e
          Right sol -> return (res, sol)
