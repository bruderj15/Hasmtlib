module Language.Hasmtlib.Solver.MathSAT where

import Language.Hasmtlib.Solver.Common
import qualified SMTLIB.Backends.Process as P

-- | A 'ProcessSolver' for MathSAT.
--   Requires binary @mathsat@ to be in path.
mathsat :: ProcessSolver
mathsat = ProcessSolver $ P.defaultConfig { P.exe = "mathsat", P.args = [] }

-- | A 'ProcessSolver' for OptiMathSAT.
--   Requires binary @optimathsat@ to be in path.
optimathsat :: ProcessSolver
optimathsat = ProcessSolver $ P.defaultConfig { P.exe = "optimathsat", P.args = ["-optimization=true"] }
