module Language.Hasmtlib.Solver.MathSAT where

import Language.Hasmtlib.Solver.Common
import qualified SMTLIB.Backends.Process as P

mathsat :: ProcessSolver
mathsat = ProcessSolver $ P.defaultConfig { P.exe = "mathsat", P.args = [] }