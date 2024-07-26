module Language.Hasmtlib.Solver.CVC5 where

import SMTLIB.Backends.Process

-- | A 'Config' for CVC5.
--   Requires binary @cvc5@ to be in path.
cvc5 :: Config
cvc5 = defaultConfig { exe = "cvc5", args = [] }
