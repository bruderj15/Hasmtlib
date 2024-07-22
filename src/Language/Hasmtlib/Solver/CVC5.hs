module Language.Hasmtlib.Solver.CVC5 where

import Language.Hasmtlib.Solver.Common
import qualified SMTLIB.Backends.Process as P

-- | A 'ProcessSolver' for CVC5.
--   Requires binary @cvc5@ to be in path.
cvc5 :: ProcessSolver
cvc5 = ProcessSolver $ P.defaultConfig { P.exe = "cvc5", P.args = [] }
