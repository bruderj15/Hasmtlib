module Language.Hasmtlib.Solver.Bitwuzla where

import SMTLIB.Backends.Process
import Language.Hasmtlib.Type.Solver

-- | A 'SolverConfig' for Bitwuzla.
--   Requires binary @bitwuzla@ to be in path.
--
--   As of v0.5 Bitwuzla uses Cadical as SAT-Solver by default.
--   Make sure it's default SAT-Solver binary - probably @cadical@ - is in path too.
bitwuzla :: SolverConfig s
bitwuzla = SolverConfig
  (defaultConfig { exe = "bitwuzla", args = [] })
  Nothing
  Nothing


-- | A 'SolverConfig' for Bitwuzla with Kissat as underlying sat-solver.
--
--   Requires binary @bitwuzla@ and to be in path.
--   Will use the @kissat@ shipped with @bitwuzla@.
--
--   It is recommended to build @bitwuzla@ from source for this to work as expected.
bitwuzlaKissat :: SolverConfig s
bitwuzlaKissat = SolverConfig
  (defaultConfig { exe = "bitwuzla", args = ["--sat-solver=kissat"] })
  Nothing
  Nothing
