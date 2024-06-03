module Language.Hasmtlib.Solver.Yices where

import Language.Hasmtlib.Type.SMT
import Language.Hasmtlib.Type.Solution
import Language.Hasmtlib.Solver.Common
import qualified SMTLIB.Backends.Process as P
import Control.Monad.State

yicesConf :: P.Config
yicesConf = P.defaultConfig { P.exe = "yices-smt2", P.args = ["--smt2-model-format"] }

yices :: MonadIO m => Solver SMT m
yices = processSolver yicesConf Nothing

yicesDebug :: MonadIO m => Solver SMT m
yicesDebug = processSolver yicesConf $ Just defaultDebugger
