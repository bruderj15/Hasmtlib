module Language.Hasmtlib.Solver.Common where

import Language.Hasmtlib.Type.SMT
import Language.Hasmtlib.Type.Expr
import Language.Hasmtlib.Type.Solution
import Language.Hasmtlib.Internal.Parser
import Data.Sequence hiding ((|>), filter)
import Data.ByteString.Lazy hiding (singleton)
import Data.ByteString.Builder
import Data.Attoparsec.ByteString
import Data.AttoLisp
import Control.Lens hiding (List)
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Text as T
import qualified SMTLIB.Backends.Process as P
import qualified SMTLIB.Backends as B hiding (Solver)

processSolver :: MonadIO m => P.Config -> Solver SMT m
processSolver cfg smt = do
  liftIO $ P.with cfg $ \handle -> do
    solver <- B.initSolver B.Queuing $ P.toBackend handle

    forM_ (buildSMT smt) (B.command_ solver)
    resultResponse <- B.command solver "(check-sat)"
    modelResponse  <- B.command solver "(get-model)"

    case parseOnly resultParser (toStrict resultResponse) of
      Left e    -> fail e
      Right res -> case res of
        Unsat -> return (res, mempty)
        _     -> case parseOnly modelParser (toStrict modelResponse) of
          Left e    -> fail e
          Right sol -> return (res, sol)
  
buildSMT :: SMT -> Seq Builder
buildSMT smt =
     fromList (fromLispExpr . toLisp <$> smt^.options)
  >< maybe mempty (\l -> singleton $ fromLispExpr (List [Symbol "set-logic", Symbol (T.pack l)])) (smt^.mlogic)
  >< buildVars (smt^.vars)
  >< fmap (\f -> fromLispExpr (List [Symbol "assert", toLisp f])) (smt^.formulas)
  
buildVars :: Seq (SomeKnownSMTRepr SMTVar) -> Seq Builder
buildVars = fmap (\(SomeKnownSMTRepr v) -> fromLispExpr (List [Symbol "declare-fun", toLisp v, Symbol "()", goSing v]))
  where
    goSing :: forall t. KnownSMTRepr t => SMTVar t -> Lisp
    goSing _ = toLisp $ singRepr @t
