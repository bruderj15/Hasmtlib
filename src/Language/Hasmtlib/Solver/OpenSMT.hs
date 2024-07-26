module Language.Hasmtlib.Solver.OpenSMT where

import SMTLIB.Backends.Process

-- | A 'Config' for OpenSMT.
--   Requires binary @opensmt@ to be in path.
opensmt :: Config
opensmt = defaultConfig { exe = "opensmt", args = [] }
