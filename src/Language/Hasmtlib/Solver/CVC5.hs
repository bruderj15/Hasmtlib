module Language.Hasmtlib.Solver.CVC5 where

import Prelude hiding (length)

import Language.Hasmtlib.Type.SMT
import Language.Hasmtlib.Type.Solution
import Language.Hasmtlib.Internal.Parser
import Language.Hasmtlib.Solver.Common
import qualified SMTLIB.Backends as B
import qualified SMTLIB.Backends.Process as P
import Data.Attoparsec.ByteString.Char8
import Data.ByteString.Lazy
import Control.Monad.State
import Control.Monad

cvc5 :: MonadIO m => Solver SMT m
cvc5 smt = do
  let myConfig = P.defaultConfig { P.exe = "cvc5", P.args = [] }
  liftIO $ P.with myConfig $ \handle -> do
    mysolver <- B.initSolver B.Queuing $ P.toBackend handle

    forM_ (buildSMT smt) (B.command_ mysolver)
    resultResponse <- B.command mysolver "(check-sat)"
    modelResponse  <- B.command mysolver "(get-model)"

    case parseOnly resultParser (toStrict resultResponse) of
      Left e    -> fail e
      Right res -> case res of
        Unsat -> return (res, mempty)
        _     -> case parseOnly modelParser (toStrict modelResponse) of
          Left e    -> fail e
          Right sol -> return (res, sol)
