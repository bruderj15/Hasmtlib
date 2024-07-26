module Language.Hasmtlib.Solver.Bitwuzla where

import SMTLIB.Backends.Process

-- | A 'Config' for Bitwuzla.
--   Requires binary @bitwuzla@ to be in path.
--
--   As of v0.5 Bitwuzla uses Cadical as SAT-Solver by default.
--   Make sure it's default SAT-Solver binary - probably @cadical@ - is in path too.
bitwuzla :: Config
bitwuzla = defaultConfig { exe = "bitwuzla", args = [] }

-- | A 'Config' for Bitwuzla which uses Kissat for SAT-Solving.
--   Requires binary @bitwuzla@ and @kissat@ to be in path.
--
-- Combination with Kissat currently behaves weirdly: https://github.com/bitwuzla/bitwuzla/issues/119
--
-- bitwuzlaWithKissat :: Config
-- bitwuzlaWithKissat = defaultConfig { exe = "bitwuzla", args = ["--sat-solver=kissat"] }
