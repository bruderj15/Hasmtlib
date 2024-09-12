module Language.Hasmtlib.Solver.Yices where

import SMTLIB.Backends.Process
import Language.Hasmtlib.Type.Solver

-- | A 'SolverConfig' for Yices.
--   Requires binary @yices-smt2@ to be in path.
yices :: SolverConfig s
yices = SolverConfig
  (defaultConfig { exe = "yices-smt2", args = ["--smt2-model-format", "--incremental"] })
  Nothing Nothing
