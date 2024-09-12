module Language.Hasmtlib.Solver.CVC5 where

import SMTLIB.Backends.Process
import Language.Hasmtlib.Type.Solver

-- | A 'SolverConfig' for CVC5.
--   Requires binary @cvc5@ to be in path.
cvc5 :: SolverConfig s
cvc5 = SolverConfig
  (defaultConfig { exe = "cvc5", args = [] })
  Nothing Nothing
