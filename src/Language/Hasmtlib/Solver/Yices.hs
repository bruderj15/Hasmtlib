module Language.Hasmtlib.Solver.Yices where

import SMTLIB.Backends.Process

-- | A 'Config' for Yices.
--   Requires binary @yices-smt2@ to be in path.
yices :: Config
yices = defaultConfig { exe = "yices-smt2", args = ["--smt2-model-format", "--incremental"] }
