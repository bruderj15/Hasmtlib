module Language.Hasmtlib.Solver.Yices where

import Language.Hasmtlib.Solver.Common
import qualified SMTLIB.Backends.Process as P

-- | A 'ProcessSolver' for Yices.
--   Requires binary @yices-smt2@ to be in path.
yices :: ProcessSolver
yices = ProcessSolver $ P.defaultConfig { P.exe = "yices-smt2", P.args = ["--smt2-model-format"] }
