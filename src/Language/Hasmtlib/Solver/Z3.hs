module Language.Hasmtlib.Solver.Z3 where

import Language.Hasmtlib.Solver.Common
import qualified SMTLIB.Backends.Process as P

-- TODO: Add support for lib binding: https://github.com/tweag/smtlib-backends/tree/master/smtlib-backends-z3

z3 :: ProcessSolver
z3 = ProcessSolver P.defaultConfig

