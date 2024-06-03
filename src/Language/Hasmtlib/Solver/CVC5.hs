module Language.Hasmtlib.Solver.CVC5 where

import Language.Hasmtlib.Type.SMT
import Language.Hasmtlib.Type.Solution
import Language.Hasmtlib.Solver.Common
import qualified SMTLIB.Backends.Process as P
import Control.Monad.State

-- TODO: Add support for lib binding: https://github.com/tweag/smtlib-backends/tree/master/smtlib-backends-cvc5

cvc5Conf :: P.Config
cvc5Conf = P.defaultConfig { P.exe = "cvc5", P.args = [] }

cvc5 :: MonadIO m => Solver SMT m
cvc5 = processSolver cvc5Conf Nothing

cvc5Debug :: MonadIO m => Solver SMT m
cvc5Debug = processSolver cvc5Conf $ Just defaultDebugger