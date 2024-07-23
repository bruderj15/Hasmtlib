module Language.Hasmtlib.Solver.Common where

import Language.Hasmtlib.Type.SMT
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
import qualified SMTLIB.Backends.Process as P
import qualified SMTLIB.Backends as B

-- | A newtype-wrapper for 'P.Config' which configures a solver via external process.
newtype ProcessSolver = ProcessSolver { conf :: P.Config }

-- | Creates a 'Solver' from a 'ProcessSolver'
solver :: MonadIO m => ProcessSolver -> Solver SMT m
solver (ProcessSolver cfg) = processSolver cfg Nothing

-- | Creates a debugging 'Solver' from a 'ProcessSolver'
debug :: MonadIO m => ProcessSolver -> Solver SMT m
debug (ProcessSolver cfg) = processSolver cfg $ Just def

-- | Creates an interactive session with a solver by creating and returning an alive process-handle 'P.Handle'.
interactiveSolver :: MonadIO m => ProcessSolver -> m (B.Solver, P.Handle)
interactiveSolver (ProcessSolver cfg) = liftIO $ do
  handle  <- P.new cfg
  liftM2 (,) (B.initSolver B.Queuing $ P.toBackend handle) (return handle)

-- | A type holding actions to execute for debugging 'SMT' solving.
data Debugger = Debugger
  { debugSMT            :: SMT -> IO ()
  , debugProblem        :: Seq Builder -> IO ()
  , debugResultResponse :: ByteString -> IO ()
  , debugModelResponse  :: ByteString -> IO ()
  }

instance Default Debugger where
  def = Debugger
    { debugSMT            = \smt -> liftIO $ do
        putStrLn $ "Vars: "       ++ show (Seq.length (smt^.vars))
        putStrLn $ "Assertions: " ++ show (Seq.length (smt^.formulas))
    , debugProblem        = liftIO . mapM_ (putStrLn . toString . toLazyByteString)
    , debugResultResponse = liftIO . putStrLn . (\s -> "\n" ++ s ++ "\n") . toString
    , debugModelResponse  = liftIO . mapM_ (putStrLn . toString) . split 13
    }

-- | A 'Solver' which holds an external process with a SMT-Solver.
--   This will:
--
-- 1. Encode the 'SMT'-problem,
--
-- 2. Start a new external process for the SMT-Solver,
--
-- 3. Send the problem to the SMT-Solver,
--
-- 4. Wait for an answer and parse it and
--
-- 5. close the process and clean up all resources.
--
processSolver :: MonadIO m => P.Config -> Maybe Debugger -> Solver SMT m
processSolver cfg debugger smt = do
  liftIO $ P.with cfg $ \handle -> do
    maybe mempty (`debugSMT` smt) debugger
    pSolver <- B.initSolver B.Queuing $ P.toBackend handle

    let problem = renderSeq smt
    maybe mempty (`debugProblem` problem) debugger

    forM_ problem (B.command_ pSolver)
    resultResponse <- B.command pSolver "(check-sat)"
    maybe mempty (`debugResultResponse` resultResponse) debugger

    modelResponse  <- B.command pSolver "(get-model)"
    maybe mempty (`debugModelResponse` modelResponse) debugger

    case parseOnly resultParser (toStrict resultResponse) of
      Left e    -> fail e
      Right res -> case res of
        Unsat -> return (res, mempty)
        _     -> case parseOnly anyModelParser (toStrict modelResponse) of
          Left e    -> fail e
          Right sol -> return (res, sol)
