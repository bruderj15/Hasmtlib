{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE LambdaCase #-}

{- |
This module provides an IO-'Pipe' to external SMT-Solvers and ships with implementations for 'MonadSMT', 'MonadOMT' and 'MonadIncrSMT'.

The 'Pipe' is based on a 'B.Solver' from Tweag's package @smtlib-backends@ and in reality is just an IO-Handle.
-}
module Language.Hasmtlib.Type.Pipe
(
  -- * Type
  Pipe(..)

  -- * Lens
, lastPipeVarId, mPipeLogic
, pipeSharingMode, pipeStableMap, incrSharedAuxs
, pipeSolver
)
where

import Language.Hasmtlib.Internal.Sharing
import Language.Hasmtlib.Internal.Render
import Language.Hasmtlib.Type.Debugger
import Language.Hasmtlib.Type.Expr
import Language.Hasmtlib.Type.OMT (SoftFormula(..), Minimize(..), Maximize(..))
import Language.Hasmtlib.Type.MonadSMT
import Language.Hasmtlib.Type.SMTSort
import Language.Hasmtlib.Type.Solution
import Language.Hasmtlib.Codec
import Language.Hasmtlib.Internal.Parser hiding (var, constant)
import qualified SMTLIB.Backends as B
import Data.HashMap.Lazy
import Data.Sequence hiding ((|>), (:>))
import Data.IntMap as IntMap (singleton)
import Data.Dependent.Map as DMap
import Data.Coerce
import Data.ByteString.Builder
import Data.ByteString.Lazy hiding (filter, singleton, isPrefixOf)
import Data.Attoparsec.ByteString hiding (Result)
import Control.Monad.State
import Control.Lens hiding (List)
import System.Mem.StableName

-- | A pipe to the solver.
--   If 'B.Solver' is 'B.Queuing' then all commands that do not expect an answer are sent to the queue.
--   All commands that expect an answer have the queue to be sent to the solver before sending the command itself.
--   If 'B.Solver' is not 'B.Queuing', all commands are sent to the solver immediately.
data Pipe = Pipe
  { _lastPipeVarId     :: {-# UNPACK #-} !Int                                 -- ^ Last Id assigned to a new var
  , _mPipeLogic        :: Maybe String                                        -- ^ Logic for the SMT-Solver
  , _pipeSharingMode   :: !SharingMode                                        -- ^ How to share common expressions
  , _pipeStableMap     :: !(HashMap (StableName ()) (SomeKnownSMTSort Expr))  -- ^ Mapping between a 'StableName' and it's 'Expr' we may share
  , _incrSharedAuxs    :: !(Seq (Seq (StableName ())))                        -- ^ Index of each 'Seq' ('StableName' ()) is incremental stack height where 'StableName' representing auxiliary var that has been shared
  , _pipeSolver        :: !B.Solver                                           -- ^ Active pipe to the backend
  , _mPipeDebugger      :: Maybe (Debugger Pipe)                                 -- ^ Debugger for communication with the external solver
  }
$(makeLenses ''Pipe)

instance Sharing Pipe where
  type SharingMonad Pipe = MonadIO
  stableMap = pipeStableMap
  assertSharedNode sn expr = do
    pipe <- get
    modifying (incrSharedAuxs._last) (|> sn)
    let cmd = renderAssert expr
    liftIO $ maybe (return ()) (`debugAssert` cmd) (pipe^.mPipeDebugger)
    liftIO $ B.command_ (pipe^.pipeSolver) cmd
  setSharingMode sm = pipeSharingMode .= sm

instance (MonadState Pipe m, MonadIO m) => MonadSMT Pipe m where
  smtvar' _ = fmap coerce $ lastPipeVarId <+= 1
  {-# INLINE smtvar' #-}

  var' p = do
    pipe <- get
    newVar <- smtvar' p
    let cmd = renderDeclareVar newVar
    liftIO $ maybe (return ()) (`debugVar` cmd) (pipe^.mPipeDebugger)
    liftIO $ B.command_ (pipe^.pipeSolver) cmd
    return $ Var newVar
  {-# INLINEABLE var' #-}

  assert expr = do
    pipe <- get
    sExpr <- runSharing (pipe^.pipeSharingMode) expr
    qExpr <- boundify sExpr
    let cmd = renderAssert qExpr
    liftIO $ maybe (return ()) (`debugAssert` cmd) (pipe^.mPipeDebugger)
    liftIO $ B.command_ (pipe^.pipeSolver) cmd
  {-# INLINEABLE assert #-}

  setOption opt = do
    pipe <- get
    let cmd = render opt
    liftIO $ maybe (return ()) (`debugOption` cmd) (pipe^.mPipeDebugger)
    liftIO $ B.command_ (pipe^.pipeSolver) cmd

  setLogic l = do
    mPipeLogic ?= l
    pipe <- get
    let cmd = renderSetLogic (stringUtf8 l)
    liftIO $ maybe (return ()) (`debugLogic` cmd) (pipe^.mPipeDebugger)
    liftIO $ B.command_ (pipe^.pipeSolver) cmd

instance (MonadState Pipe m, MonadIO m) => MonadIncrSMT Pipe m where
  push = do
    pipe <- get
    let cmd = renderPush 1
    liftIO $ maybe (return ()) (`debugPush` cmd) (pipe^.mPipeDebugger)
    liftIO $ B.command_ (pipe^.pipeSolver) cmd
    incrSharedAuxs <>= mempty

  pop = do
    pipe <- get
    let cmd = renderPop 1
    liftIO $ maybe (return ()) (`debugPop` cmd) (pipe^.mPipeDebugger)
    liftIO $ B.command_ (pipe^.pipeSolver) cmd
    forMOf_ (incrSharedAuxs._last.folded) pipe (\sn -> pipeStableMap.at sn .= Nothing)
    modifying incrSharedAuxs $ \case (auxs:>_) -> auxs ; auxs -> auxs

  checkSat = do
    pipe <- get
    let cmd = renderCheckSat
    liftIO $ maybe (return ()) (`debugCheckSat` cmd) (pipe^.mPipeDebugger)
    result <- liftIO $ B.command (pipe^.pipeSolver) cmd
    liftIO $ maybe (return ()) (`debugResultResponse` result) (pipe^.mPipeDebugger)
    case parseOnly resultParser (toStrict result) of
      Left e    -> liftIO $ do
        print result
        error e
      Right res -> return res

  getModel = do
    pipe   <- get
    let cmd = renderGetModel
    liftIO $ maybe (return ()) (`debugGetModel` cmd) (pipe^.mPipeDebugger)
    model <- liftIO $ B.command (pipe^.pipeSolver) cmd
    liftIO $ maybe (return ()) (`debugModelResponse` model) (pipe^.mPipeDebugger)
    case parseOnly anyModelParser (toStrict model) of
      Left e    -> liftIO $ do
        print model
        error e
      Right sol -> return sol

  getValue :: forall t. KnownSMTSort t => Expr t -> m (Maybe (Decoded (Expr t)))
  getValue v@(Var x) = do
    pipe   <- get
    let cmd = renderGetValue x
    liftIO $ maybe (return ()) (`debugGetValue` cmd) (pipe^.mPipeDebugger)
    model <- liftIO $ B.command (pipe^.pipeSolver) cmd
    liftIO $ maybe (return ()) (`debugModelResponse` model) (pipe^.mPipeDebugger)
    case parseOnly (getValueParser @t x) (toStrict model) of
      Left e    -> liftIO $ do
        print model
        error e
      Right sol ->
        return $
          decode
            (DMap.singleton
              (sortSing @t)
              (IntValueMap $ IntMap.singleton (sol^.solVar.varId) (sol^.solVal)))
            v
  getValue expr = do
    model <- getModel
    return $ decode model expr

instance (MonadSMT Pipe m, MonadIO m) => MonadOMT Pipe m where
  minimize expr = do
    pipe <- get
    sExpr <- runSharing (pipe^.pipeSharingMode) expr
    let cmd = render $ Minimize sExpr
    liftIO $ maybe (return ()) (`debugMinimize` cmd) (pipe^.mPipeDebugger)
    liftIO $ B.command_ (pipe^.pipeSolver) cmd
  {-# INLINEABLE minimize #-}

  maximize expr = do
    pipe <- get
    sExpr <- runSharing (pipe^.pipeSharingMode) expr
    let cmd = render $ Maximize sExpr
    liftIO $ maybe (return ()) (`debugMaximize` cmd) (pipe^.mPipeDebugger)
    liftIO $ B.command_ (pipe^.pipeSolver) cmd
  {-# INLINEABLE maximize #-}

  assertSoft expr w gid = do
    pipe <- get
    sExpr <- runSharing (pipe^.pipeSharingMode) expr
    let cmd = render $ SoftFormula sExpr w gid
    liftIO $ maybe (return ()) (`debugAssertSoft` cmd) (pipe^.mPipeDebugger)
    liftIO $ B.command_ (pipe^.pipeSolver) cmd
  {-# INLINEABLE assertSoft #-}
