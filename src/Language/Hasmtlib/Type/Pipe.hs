{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Hasmtlib.Type.Pipe
 ( Pipe, withSolver
 , push, pop
 , solve, checkSat, getModel, getValue
 )
 where

import Language.Hasmtlib.Type.SMT
import Language.Hasmtlib.Type.MonadSMT
import Language.Hasmtlib.Internal.Expr
import Language.Hasmtlib.Internal.Render
import Language.Hasmtlib.Type.Solution
import Language.Hasmtlib.Codec
import Language.Hasmtlib.Internal.Parser hiding (var, constant)
import qualified SMTLIB.Backends as B
import Data.IntMap (singleton)
import Data.Coerce
import Data.ByteString.Builder
import Data.ByteString.Lazy hiding (filter, singleton)
import Data.Attoparsec.ByteString hiding (Result)
import Control.Monad
import Control.Monad.State
import Control.Lens hiding (List)

-- | Pipe to the solver.
-- | If @B.Solver@ is @B.Queuing@ then all commands but those that expect an answer are sent to the queue.
data Pipe = Pipe
  { _lastPipeVarId :: {-# UNPACK #-} !Int
  , _pipe          :: !B.Solver
  }

$(makeLenses ''Pipe)

withSolver :: B.Solver -> Pipe
withSolver = Pipe 0

instance (MonadState Pipe m, MonadIO m) => MonadSMT Pipe m where
  smtvar' _ = fmap coerce $ lastPipeVarId <+= 1
  {-# INLINE smtvar' #-}

  var' p = do
    smt <- get
    newVar <- smtvar' p
    liftIO $ B.command_ (smt^.pipe) $ renderDeclareVar newVar
    return $ Var newVar
  {-# INLINEABLE var' #-}

  assert expr = do
    smt <- get
    qExpr <- quantify expr
    liftIO $ B.command_ (smt^.pipe) $ renderAssert qExpr
  {-# INLINEABLE assert #-}

  setOption opt = do
    smt <- get
    liftIO $ B.command_ (smt^.pipe) $ renderSMTLib2 opt

  setLogic l = do
    smt <- get
    liftIO $ B.command_ (smt^.pipe) $ renderSetLogic (stringUtf8 l)

-- | Push a new context to the solvers context-stack.
push :: (MonadSMT Pipe m, MonadIO m) => m ()
push = do
  smt <- get
  liftIO $ B.command_ (smt^.pipe) "(push 1)"

-- | Pop the solver context-stack.
pop :: (MonadSMT Pipe m, MonadIO m) => m ()
pop = do
  smt <- get
  liftIO $ B.command_ (smt^.pipe) "(pop 1)"

-- | Run check-sat and get-model on the current problem
solve :: (MonadSMT Pipe m, MonadIO m) => m (Result, Solution)
solve = liftM2 (,) checkSat getModel

-- | Run check-sat on the current problem
checkSat :: forall m. (MonadSMT Pipe m, MonadIO m) => m Result
checkSat = do
  smt <- get
  result <- liftIO $ B.command (smt^.pipe) "(check-sat)"
  case parseOnly resultParser (toStrict result) of
    Left e    -> liftIO $ do
      print result
      error e
    Right res -> return res

-- | Run get-model on the current problem
getModel :: (MonadSMT Pipe m, MonadIO m) => m Solution
getModel = do
  smt   <- get
  model <- liftIO $ B.command (smt^.pipe) "(get-model)"
  case parseOnly anyModelParser (toStrict model) of
    Left e    -> liftIO $ do
      print model
      error e
    Right sol -> return sol

-- | Evaluate any expressions value in the solvers model.
--   Requires a SAT or UNKNOWN check-sat response beforehand.
getValue ::  forall m t. (MonadSMT Pipe m, MonadIO m, KnownSMTRepr t) => Expr t -> m (Maybe (Decoded (Expr t)))
getValue v@(Var x) = do
  smt   <- get
  model <- liftIO $ B.command (smt^.pipe) $ renderUnary "get-value" $ "(" <> renderSMTLib2 x <> ")"
  case parseOnly (getValueParser @t x) (toStrict model) of
    Left e    -> liftIO $ do
      print model
      error e
    Right sol -> return $ decode (singleton (sol^.solVar.varId) (SomeKnownSMTRepr sol)) v
getValue expr = do
  model <- getModel
  return $ decode model expr