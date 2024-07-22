module Language.Hasmtlib.Solver.CVC5 where

import Language.Hasmtlib.Solver.Common
import qualified SMTLIB.Backends.Process as P

opensmt :: ProcessSolver
opensmt = ProcessSolver $ P.defaultConfig { P.exe = "opensmt", P.args = [] }
