module Language.Hasmtlib.Solver.CVC5 where

import Language.Hasmtlib.Type.SMT
import Language.Hasmtlib.Type.Solution
import Language.Hasmtlib.Solver.Common
import Data.Default
import qualified SMTLIB.Backends.Process as P
import qualified SMTLIB.Backends as B
import Control.Monad
import Control.Monad.State

-- TODO: Add support for lib binding: https://github.com/tweag/smtlib-backends/tree/master/smtlib-backends-cvc5

cvc5Conf :: P.Config
cvc5Conf = P.defaultConfig { P.exe = "cvc5", P.args = [] }

cvc5 :: MonadIO m => Solver SMT m
cvc5 = processSolver cvc5Conf Nothing

cvc5Debug :: MonadIO m => Solver SMT m
cvc5Debug = processSolver cvc5Conf $ Just def

cvc5Alive :: MonadIO m => m (B.Solver, P.Handle)
cvc5Alive = liftIO $ do
  handle  <- P.new cvc5Conf
  liftM2 (,) (B.initSolver B.Queuing $ P.toBackend handle) (return handle)