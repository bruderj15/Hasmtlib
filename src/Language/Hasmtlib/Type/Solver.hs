module Language.Hasmtlib.Type.Solver where

import qualified SMTLIB.Backends as B
import qualified SMTLIB.Backends.Process as P
import Language.Hasmtlib.Type.SMT
import Language.Hasmtlib.Codec
import Language.Hasmtlib.Problem
import Prelude hiding (putStr)
import Data.Default
import Data.ByteString.Builder
import Control.Monad.State

type Solver s m = s -> m Solution

solveWith :: (MonadState SMT m, Default s, Codec a) => Solver s m -> StateT s m a -> m (Decoded a)
solveWith solve m = do
  (a, problem) <- runStateT m def
  solution <- solve problem
  
  -- TODO: Convert solution ByteString to structured solution
  
  return $ decode solution a
  
mySolver :: MonadIO m => Solver SMT m
mySolver smt = do
  let myConfig = P.defaultConfig { P.exe = "cvc5", P.args = [] }
  liftIO $ P.with myConfig $ \handle -> do
    mysolver <- B.initSolver B.NoQueuing $ P.toBackend handle
    B.command mysolver $ lazyByteString $ buildSMT smt
