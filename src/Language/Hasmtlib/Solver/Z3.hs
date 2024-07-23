module Language.Hasmtlib.Solver.Z3 where

import Language.Hasmtlib.Solver.Common
import qualified SMTLIB.Backends.Process as P

-- | A 'ProcessSolver' for Z3.
--   Requires binary @z3@ to be in path.
z3 :: ProcessSolver
z3 = ProcessSolver P.defaultConfig
