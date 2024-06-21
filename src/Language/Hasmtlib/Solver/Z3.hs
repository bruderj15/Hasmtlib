module Language.Hasmtlib.Solver.Z3 where

import Language.Hasmtlib.Solver.Common
import qualified SMTLIB.Backends.Process as P

z3 :: ProcessSolver
z3 = ProcessSolver P.defaultConfig

