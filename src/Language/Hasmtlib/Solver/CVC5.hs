module Language.Hasmtlib.Solver.CVC5 where

import Language.Hasmtlib.Solver.Common
import qualified SMTLIB.Backends.Process as P

-- TODO: Add support for lib binding: https://github.com/tweag/smtlib-backends/tree/master/smtlib-backends-cvc5

cvc5 :: ProcessSolver
cvc5 = ProcessSolver $ P.defaultConfig { P.exe = "cvc5", P.args = [] }
