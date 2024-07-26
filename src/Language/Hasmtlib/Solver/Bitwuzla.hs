module Language.Hasmtlib.Solver.Bitwuzla where

import Language.Hasmtlib.Solver.Common
import qualified SMTLIB.Backends.Process as P

-- | A 'ProcessSolver' for Bitwuzla.
--   Requires binary @bitwuzla@ to be in path.
--
--   As of v0.5 Bitwuzla uses Cadical as SAT-Solver by default.
--   Make sure it's default SAT-Solver - probably @cadical@ - is in path too.
bitwuzla :: ProcessSolver
bitwuzla = ProcessSolver $ P.defaultConfig { P.exe = "bitwuzla", P.args = [] }

-- | A 'ProcessSolver' for Bitwuzla.
--   Requires binary @bitwuzla@ and @kissat@ to be in path.
--
-- Combination with Kissat currently behaves weirdly: https://github.com/bitwuzla/bitwuzla/issues/119
--
-- bitwuzlaWithKissat :: ProcessSolver
-- bitwuzlaWithKissat = ProcessSolver $ P.defaultConfig { P.exe = "bitwuzla", P.args = ["--sat-solver=kissat"] }
