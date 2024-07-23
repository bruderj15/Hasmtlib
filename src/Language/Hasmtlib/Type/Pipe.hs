{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Hasmtlib.Type.Pipe where

import Language.Hasmtlib.Type.SMT
import Language.Hasmtlib.Type.OMT (SoftFormula(..), Minimize(..), Maximize(..))
import Language.Hasmtlib.Type.MonadSMT
import Language.Hasmtlib.Internal.Expr
import Language.Hasmtlib.Internal.Render
import Language.Hasmtlib.Type.SMTSort
import Language.Hasmtlib.Type.Solution
import Language.Hasmtlib.Codec
import Language.Hasmtlib.Internal.Parser hiding (var, constant)
import qualified SMTLIB.Backends as B
import Data.List (isPrefixOf)
import Data.IntMap as IMap (singleton)
import Data.Dependent.Map as DMap
import Data.Coerce
import Data.ByteString.Builder
import Data.ByteString.Lazy hiding (filter, singleton, isPrefixOf)
import Data.Attoparsec.ByteString hiding (Result)
import Control.Monad.State
import Control.Lens hiding (List)

-- | A pipe to the solver.
--   If 'B.Solver' is 'B.Queuing' then all commands that do not expect an answer are sent to the queue.
--   All commands that expect an answer have the queue to be sent to the solver before sending the command itself.
--   If 'B.Solver' is not 'B.Queuing', all commands are sent to the solver immediately.
data Pipe = Pipe
  { _lastPipeVarId :: {-# UNPACK #-} !Int              -- ^ Last Id assigned to a new var
  , _mPipeLogic    :: Maybe String                     -- ^ Logic for the SMT-Solver
  , _pipe          :: !B.Solver                        -- ^ Active pipe to the backend
  }

$(makeLenses ''Pipe)

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
    qExpr <- case smt^.mPipeLogic of
      Nothing    -> return expr
      Just logic -> if "QF" `isPrefixOf` logic then return expr else quantify expr
    liftIO $ B.command_ (smt^.pipe) $ renderAssert qExpr
  {-# INLINEABLE assert #-}

  setOption opt = do
    smt <- get
    liftIO $ B.command_ (smt^.pipe) $ render opt

  setLogic l = do
    mPipeLogic ?= l
    smt <- get
    liftIO $ B.command_ (smt^.pipe) $ renderSetLogic (stringUtf8 l)

instance (MonadState Pipe m, MonadIO m) => MonadIncrSMT Pipe m where
  push = do
    smt <- get
    liftIO $ B.command_ (smt^.pipe) "(push 1)"
  {-# INLINE push #-}

  pop = do
    smt <- get
    liftIO $ B.command_ (smt^.pipe) "(pop 1)"
  {-# INLINE pop #-}

  checkSat = do
    smt <- get
    result <- liftIO $ B.command (smt^.pipe) "(check-sat)"
    case parseOnly resultParser (toStrict result) of
      Left e    -> liftIO $ do
        print result
        error e
      Right res -> return res

  getModel = do
    smt   <- get
    model <- liftIO $ B.command (smt^.pipe) "(get-model)"
    case parseOnly anyModelParser (toStrict model) of
      Left e    -> liftIO $ do
        print model
        error e
      Right sol -> return sol

  getValue :: forall t. KnownSMTSort t => Expr t -> m (Maybe (Decoded (Expr t)))
  getValue v@(Var x) = do
    smt   <- get
    model <- liftIO $ B.command (smt^.pipe) $ renderUnary "get-value" $ "(" <> render x <> ")"
    case parseOnly (getValueParser @t x) (toStrict model) of
      Left e    -> liftIO $ do
        print model
        error e
      Right sol ->
        return $
          decode
            (DMap.singleton
              (sortSing @t)
              (IntValueMap $ IMap.singleton (sol^.solVar.varId) (sol^.solVal)))
            v
  getValue expr = do
    model <- getModel
    return $ decode model expr
  {-# INLINEABLE getValue #-}

instance (MonadSMT Pipe m, MonadIO m) => MonadOMT Pipe m where
  minimize expr = do
    smt <- get
    liftIO $ B.command_ (smt^.pipe) $ render $ Minimize expr
  {-# INLINEABLE minimize #-}

  maximize expr = do
    smt <- get
    liftIO $ B.command_ (smt^.pipe) $ render $ Maximize expr
  {-# INLINEABLE maximize #-}

  assertSoft expr w gid = do
    smt <- get
    liftIO $ B.command_ (smt^.pipe) $ render $ SoftFormula expr w gid
