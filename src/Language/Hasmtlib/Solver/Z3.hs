module Language.Hasmtlib.Solver.Z3 where

import SMTLIB.Backends.Process

-- | A 'Config' for Z3.
--   Requires binary @z3@ to be in path.
z3 :: Config
z3 = defaultConfig
