{-# LANGUAGE OverloadedStrings #-}

module Language.Hasmtlib.Solver.CVC5 where

import Language.Hasmtlib.Type.SMT
import Language.Hasmtlib.Type.Solution
import Language.Hasmtlib.Internal.Parser
import Language.Hasmtlib.Solver.Common
import qualified SMTLIB.Backends as B
import qualified SMTLIB.Backends.Process as P
import Data.Attoparsec.ByteString.Char8
import Data.ByteString.Builder
import Data.ByteString.Lazy
import Control.Monad.State

cvc5 :: MonadIO m => Solver SMT m
cvc5 smt = do
  let myConfig = P.defaultConfig { P.exe = "cvc5", P.args = [] }
  liftIO $ P.with myConfig $ \handle -> do
    mysolver <- B.initSolver B.NoQueuing $ P.toBackend handle
    answer <- B.command mysolver $ lazyByteString $ buildSMT smt

    liftIO $ print answer

    case parseOnly answerParser (toStrict answer) of
      Left e  -> fail e
      Right res -> return res
