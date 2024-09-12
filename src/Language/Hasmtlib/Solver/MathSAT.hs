module Language.Hasmtlib.Solver.MathSAT where

import SMTLIB.Backends.Process
import Language.Hasmtlib.Type.Solver

-- | A 'SolverConfig' for MathSAT.
--   Requires binary @mathsat@ to be in path.
mathsat :: SolverConfig s
mathsat = SolverConfig
  (defaultConfig { exe = "mathsat", args = [] })
  Nothing
  Nothing

-- | A 'SolverConfig' for OptiMathSAT.
--   Requires binary @optimathsat@ to be in path.
optimathsat :: SolverConfig s
optimathsat = SolverConfig
  (defaultConfig { exe = "optimathsat", args = ["-optimization=true"] })
  Nothing
  Nothing
