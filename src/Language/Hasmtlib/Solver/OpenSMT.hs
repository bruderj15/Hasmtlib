module Language.Hasmtlib.Solver.OpenSMT where

import Language.Hasmtlib.Solver.Common
import qualified SMTLIB.Backends.Process as P

-- | A 'ProcessSolver' for OpenSMT.
--   Requires binary @opensmt@ to be in path.
opensmt :: ProcessSolver
opensmt = ProcessSolver $ P.defaultConfig { P.exe = "opensmt", P.args = [] }
