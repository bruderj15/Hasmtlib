module Language.Hasmtlib.Solver.Common where

import Language.Hasmtlib.Type.SMT
import Language.Hasmtlib.Type.Solution
import Language.Hasmtlib.Internal.Parser
import Language.Hasmtlib.Internal.Render
import Data.Default
import Data.Sequence hiding ((|>), filter)
import Data.ByteString.Lazy hiding (singleton)
import Data.ByteString.Lazy.UTF8 (toString)
import Data.ByteString.Builder
import Data.Attoparsec.ByteString
import Control.Monad
import Control.Monad.IO.Class
import qualified SMTLIB.Backends.Process as P
import qualified SMTLIB.Backends as B hiding (Solver)

data Debugger = Debugger {
    debugSMT            :: SMT -> IO ()
  , debugProblem        :: Seq Builder -> IO ()
  , debugResultResponse :: ByteString -> IO ()
  , debugModelResponse  :: ByteString -> IO ()
  }

instance Default Debugger where
  def = Debugger
    { debugSMT            = const $ return ()
    , debugProblem        = liftIO . mapM_ (putStrLn . toString . toLazyByteString)
    , debugResultResponse = liftIO . putStrLn . (\s -> "\n" ++ s ++ "\n") . toString
    , debugModelResponse  = liftIO . mapM_ (putStrLn . toString) . split 13
    }

-- | Use an external process for solver
processSolver :: MonadIO m => P.Config -> Maybe Debugger -> Solver SMT m
processSolver cfg debugger smt = do
  liftIO $ P.with cfg $ \handle -> do
    maybe mempty (`debugSMT` smt) debugger
    solver <- B.initSolver B.Queuing $ P.toBackend handle

    let problem = renderSMT smt
    maybe mempty (`debugProblem` problem) debugger

    forM_ problem (B.command_ solver)
    resultResponse <- B.command solver renderCheckSat
    maybe mempty (`debugResultResponse` resultResponse) debugger

    modelResponse  <- B.command solver renderGetModel
    maybe mempty (`debugModelResponse` modelResponse) debugger

    case parseOnly resultParser (toStrict resultResponse) of
      Left e    -> fail e
      Right res -> case res of
        Unsat -> return (res, mempty)
        _     -> case parseOnly anyModelParser (toStrict modelResponse) of
          Left e    -> fail e
          Right sol -> return (res, sol)
