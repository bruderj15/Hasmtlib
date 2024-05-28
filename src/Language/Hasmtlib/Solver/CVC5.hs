module Language.Hasmtlib.Solver.CVC5 where

import Language.Hasmtlib.Type.SMT
import Language.Hasmtlib.Type.Solution
import Language.Hasmtlib.Solver.Common
import qualified SMTLIB.Backends.Process as P
import Control.Monad.State

-- TODO: Add support for lib binding: https://github.com/tweag/smtlib-backends/tree/master/smtlib-backends-cvc5

cvc5 :: MonadIO m => Solver SMT m
cvc5 = processSolver cfg
  where cfg = P.defaultConfig { P.exe = "cvc5", P.args = [] }
