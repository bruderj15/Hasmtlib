module Language.Hasmtlib.Solver.Z3 where

import SMTLIB.Backends.Process
import Language.Hasmtlib.Type.Solver

-- | A 'SolverConfig' for Z3.
--   Requires binary @z3@ to be in path.
z3 :: SolverConfig s
z3 = SolverConfig
  defaultConfig
  Nothing Nothing
