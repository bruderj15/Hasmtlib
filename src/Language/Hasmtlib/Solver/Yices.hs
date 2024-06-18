module Language.Hasmtlib.Solver.Yices where

import Language.Hasmtlib.Solver.Common
import qualified SMTLIB.Backends.Process as P

yices :: ProcessSolver
yices = ProcessSolver $ P.defaultConfig { P.exe = "yices-smt2", P.args = ["--smt2-model-format"] }