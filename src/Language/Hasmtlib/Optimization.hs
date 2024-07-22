module Language.Hasmtlib.Optimization where

import Language.Hasmtlib.Type.Pipe
import Language.Hasmtlib.Type.MonadSMT
import Language.Hasmtlib.Internal.Expr
import Language.Hasmtlib.Internal.Render
import qualified SMTLIB.Backends as B
import Control.Monad.IO.Class
import Control.Monad.State
import Control.Lens

-- | Minimize a numerical expression within the SMT-Problem.
--   This is a MaxSMT/OMT operation.
--
--   __Caution:__ Currently we do not restrict usage of this function to solvers who actually support it.
--   When using 'minimize', make sure your solver supports MaxSMT/OMT.
minimize :: (MonadIncrSMT Pipe m, MonadIO m, KnownSMTSort t, Num (Expr t)) => Expr t -> m ()
minimize expr = do
  smt <- get
  liftIO $ B.command_ (smt^.pipe) $ "(minimize " <> render expr <> ")"

-- | Maximize a numerical expression within the SMT-Problem.
--   This is a MaxSMT/OMT operation.
--
--   __Caution:__ Currently we do not restrict usage of this function to solvers who actually support it.
--   When using 'maximize', make sure your solver supports MaxSMT/OMT.
maximize :: (MonadIncrSMT Pipe m, MonadIO m, KnownSMTSort t, Num (Expr t)) => Expr t -> m ()
maximize expr = do
  smt <- get
  liftIO $ B.command_ (smt^.pipe) $ "(maximize " <> render expr <> ")"
