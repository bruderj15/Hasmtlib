module Language.Hasmtlib.Solver.Mathsat where

import Language.Hasmtlib.Type.SMT
import Language.Hasmtlib.Type.Solution
import Language.Hasmtlib.Solver.Common
import qualified SMTLIB.Backends.Process as P
import Control.Monad.State

mathsat :: MonadIO m => Solver SMT m
mathsat = processSolver cfg
  where cfg = P.defaultConfig { P.exe = "mathsat", P.args = [] }
