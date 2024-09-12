module Language.Hasmtlib.Solver.OpenSMT where

import SMTLIB.Backends.Process
import Language.Hasmtlib.Type.Solver

-- | A 'SolverConfig' for OpenSMT.
--   Requires binary @opensmt@ to be in path.
opensmt :: SolverConfig s
opensmt = SolverConfig
  (defaultConfig { exe = "opensmt", args = [] })
  Nothing Nothing
