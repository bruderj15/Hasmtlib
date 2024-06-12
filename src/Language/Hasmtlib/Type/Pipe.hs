{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Hasmtlib.Type.Pipe
 ( Pipe, withSolver
 , SMTMonad(..)
 , push, pop
 , solve, checkSat, getModel, getValue 
 )
 where

import Language.Hasmtlib.Type.SMT
import Language.Hasmtlib.Internal.Expr
import Language.Hasmtlib.Internal.Render
import Language.Hasmtlib.Type.Solution
import Language.Hasmtlib.Codec
import Language.Hasmtlib.Internal.Parser hiding (var, constant)
import qualified SMTLIB.Backends as B
import Data.Coerce
import Data.ByteString.Builder
import Data.ByteString.Lazy hiding (filter, singleton)
import Data.Attoparsec.ByteString hiding (Result)
import Control.Monad
import Control.Monad.State
import Control.Lens hiding (List)

data Pipe = Pipe
  { _lastPipeVarId :: {-# UNPACK #-} !Int
  , _pipe          :: !B.Solver
  }

$(makeLenses ''Pipe)

withSolver :: B.Solver -> Pipe
withSolver = Pipe 0

instance (MonadState Pipe m, MonadIO m) => SMTMonad Pipe m where
  var' _ = do
    smt <- get
    let la' = smt^.lastPipeVarId + 1
        newVar = coerce la'
    liftIO $ B.command_ (smt^.pipe) $ renderDeclareVar newVar
    modify $ \ s -> s & lastPipeVarId %~ (+1)
    return $ Var newVar

  assert expr = do
    smt <- get
    liftIO $ B.command_ (smt^.pipe) $ renderAssert expr

  setOption opt = do
    smt <- get
    liftIO $ B.command_ (smt^.pipe) $ renderSMTLib2 opt

  setLogic l = do
    smt <- get
    liftIO $ B.command_ (smt^.pipe) $ renderSetLogic (stringUtf8 l)

-- | Push a new context to the solvers context-stack.
push :: (SMTMonad Pipe m, MonadIO m) => m ()
push = do
  smt <- get
  liftIO $ B.command_ (smt^.pipe) "(push 1)"

-- | Pop the solver context-stack.
pop :: (SMTMonad Pipe m, MonadIO m) => m ()
pop = do
  smt <- get
  liftIO $ B.command_ (smt^.pipe) "(pop 1)"

-- | Run check-sat and get-model on the current problem
solve :: (SMTMonad Pipe m, MonadIO m) => m (Result, Solution)
solve = liftM2 (,) checkSat getModel

-- | Run check-sat on the current problem
checkSat :: forall m. (SMTMonad Pipe m, MonadIO m) => m Result
checkSat = do
  smt <- get
  result <- liftIO $ B.command (smt^.pipe) "(check-sat)"
  case parseOnly resultParser (toStrict result) of
    Left e    -> liftIO $ do
      print result
      error e
    Right res -> return res

-- | Run get-model on the current problem
getModel :: (SMTMonad Pipe m, MonadIO m) => m Solution
getModel = do
  smt   <- get
  model <- liftIO $ B.command (smt^.pipe) "(get-model)"
  case parseOnly anyModelParser (toStrict model) of
    Left e    -> liftIO $ do
      print model
      error e
    Right sol -> return sol

-- TODO: Split cases for performance: (Var x) -> custom parser ;  _ -> getModel => decode:
-- | Evaluate any expressions value in the solvers model.
--   Requires a SAT or UNKNOWN check-sat response beforehand.
getValue :: ((SMTMonad Pipe m), MonadIO m, KnownSMTRepr t) => Expr t -> m (Maybe (Decoded (Expr t)))
getValue expr = do
  model <- getModel
  return $ decode model expr