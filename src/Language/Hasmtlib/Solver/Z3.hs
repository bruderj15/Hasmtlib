module Language.Hasmtlib.Solver.Z3 where

import Language.Hasmtlib.Type.SMT
import Language.Hasmtlib.Type.Solution
import Language.Hasmtlib.Solver.Common
import qualified SMTLIB.Backends.Process as P
import Control.Monad.State

-- TODO: Add support for lib binding: https://github.com/tweag/smtlib-backends/tree/master/smtlib-backends-z3

z3 :: MonadIO m => Solver SMT m
z3 = processSolver P.defaultConfig
