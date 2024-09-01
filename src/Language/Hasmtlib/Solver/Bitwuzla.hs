module Language.Hasmtlib.Solver.Bitwuzla where

import SMTLIB.Backends.Process

-- | A 'Config' for Bitwuzla.
--   Requires binary @bitwuzla@ to be in path.
--
--   As of v0.5 Bitwuzla uses Cadical as SAT-Solver by default.
--   Make sure it's default SAT-Solver binary - probably @cadical@ - is in path too.
bitwuzla :: Config
bitwuzla = defaultConfig { exe = "bitwuzla", args = [] }
