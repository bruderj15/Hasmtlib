module Language.Hasmtlib.Solver.MathSAT where

import SMTLIB.Backends.Process

-- | A 'Config' for MathSAT.
--   Requires binary @mathsat@ to be in path.
mathsat :: Config
mathsat = defaultConfig { exe = "mathsat", args = [] }

-- | A 'Config' for OptiMathSAT.
--   Requires binary @optimathsat@ to be in path.
optimathsat :: Config
optimathsat = defaultConfig { exe = "optimathsat", args = ["-optimization=true"] }
