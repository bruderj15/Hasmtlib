module Language.Hasmtlib.Solver.Yices where

import Language.Hasmtlib.Type.SMT
import Language.Hasmtlib.Type.Solution
import Language.Hasmtlib.Solver.Common
import qualified SMTLIB.Backends.Process as P
import Control.Monad.State

yices :: MonadIO m => Solver SMT m
yices = processSolver cfg
  where cfg = P.defaultConfig { P.exe = "yices-smt2", P.args = [] }
  