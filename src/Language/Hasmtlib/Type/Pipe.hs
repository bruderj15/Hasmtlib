{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Hasmtlib.Type.Pipe where

import Language.Hasmtlib.Internal.Sharing
import Language.Hasmtlib.Internal.Render
import Language.Hasmtlib.Type.Expr
import Language.Hasmtlib.Type.SMT
import Language.Hasmtlib.Type.OMT (SoftFormula(..), Minimize(..), Maximize(..))
import Language.Hasmtlib.Type.MonadSMT
import Language.Hasmtlib.Type.SMTSort
import Language.Hasmtlib.Type.Solution
import Language.Hasmtlib.Codec
import Language.Hasmtlib.Internal.Parser hiding (var, constant)
import qualified SMTLIB.Backends as B
import Data.HashMap.Lazy
import Data.Sequence hiding ((|>))
import Data.List (isPrefixOf)
import Data.IntMap as IntMap (singleton, IntMap)
import Data.Dependent.Map as DMap
import Data.Coerce
import qualified Data.ByteString.Lazy.Char8 as ByteString.Char8
import Data.ByteString.Builder
import Data.ByteString.Lazy hiding (filter, singleton, isPrefixOf)
import Data.Attoparsec.ByteString hiding (Result)
import Control.Monad.State
import Control.Monad
import Control.Lens hiding (List)
import System.Mem.StableName

-- | A pipe to the solver.
--   If 'B.Solver' is 'B.Queuing' then all commands that do not expect an answer are sent to the queue.
--   All commands that expect an answer have the queue to be sent to the solver before sending the command itself.
--   If 'B.Solver' is not 'B.Queuing', all commands are sent to the solver immediately.
data Pipe = Pipe
  { _lastPipeVarId     :: {-# UNPACK #-} !Int                                 -- ^ Last Id assigned to a new var
  , _mPipeLogic        :: Maybe String                                        -- ^ Logic for the SMT-Solver
  , _incrStackHeight   :: {-# UNPACK #-} !Int                                 -- ^ Current height of the incremental stack
  , _pipeStableMap     :: !(HashMap (StableName ()) (SomeKnownSMTSort Expr))  -- ^ Mapping between a 'StableName' and it's 'Expr' we may share
  , _stackHeightAuxMap :: !(IntMap (Seq (StableName ())))                     -- ^ Mapping between the stack height and all 'StableName's of expressions that had been shared on that stack height
  , _pipe              :: !B.Solver                                           -- ^ Active pipe to the backend
  , _isDebugging       :: !Bool                                               -- ^ Flag if pipe shall debug
  }
$(makeLenses ''Pipe)

instance Sharing Pipe where
  type SharingMonad Pipe = MonadIO
  stableMap = pipeStableMap
  assertSharedNode sn expr = do
    smt <- get
    stackHeightAuxMap.at (smt^.incrStackHeight).non mempty %= (|> sn)
    let cmd = renderAssert expr
    when (smt^.isDebugging) $ liftIO $ ByteString.Char8.putStrLn $ toLazyByteString cmd
    liftIO $ B.command_ (smt^.pipe) cmd

instance (MonadState Pipe m, MonadIO m) => MonadSMT Pipe m where
  smtvar' _ = fmap coerce $ lastPipeVarId <+= 1
  {-# INLINE smtvar' #-}

  var' p = do
    smt <- get
    newVar <- smtvar' p
    let cmd = renderDeclareVar newVar
    when (smt^.isDebugging) $ liftIO $ ByteString.Char8.putStrLn $ toLazyByteString cmd
    liftIO $ B.command_ (smt^.pipe) cmd
    return $ Var newVar
  {-# INLINEABLE var' #-}

  assert expr = do
    smt <- get
    sExpr <- runSharing expr
    qExpr <- case smt^.mPipeLogic of
      Nothing    -> return sExpr
      Just logic -> if "QF" `isPrefixOf` logic then return sExpr else quantify sExpr
    let cmd = renderAssert qExpr
    when (smt^.isDebugging) $ liftIO $ ByteString.Char8.putStrLn $ toLazyByteString cmd
    liftIO $ B.command_ (smt^.pipe) cmd
  {-# INLINEABLE assert #-}

  setOption opt = do
    smt <- get
    let cmd = render opt
    when (smt^.isDebugging) $ liftIO $ ByteString.Char8.putStrLn $ toLazyByteString cmd
    liftIO $ B.command_ (smt^.pipe) cmd

  setLogic l = do
    mPipeLogic ?= l
    smt <- get
    let cmd = renderSetLogic (stringUtf8 l)
    when (smt^.isDebugging) $ liftIO $ ByteString.Char8.putStrLn $ toLazyByteString cmd
    liftIO $ B.command_ (smt^.pipe) cmd

instance (MonadState Pipe m, MonadIO m) => MonadIncrSMT Pipe m where
  push = do
    smt <- get
    let cmd = "(push 1)"
    when (smt^.isDebugging) $ liftIO $ ByteString.Char8.putStrLn $ toLazyByteString cmd
    liftIO $ B.command_ (smt^.pipe) cmd
    incrStackHeight += 1

  pop = do
    smt <- get
    let cmd = "(pop 1)"
    when (smt^.isDebugging) $ liftIO $ ByteString.Char8.putStrLn $ toLazyByteString cmd
    liftIO $ B.command_ (smt^.pipe) cmd
    forMOf_ (stackHeightAuxMap.ix (smt^.incrStackHeight).folded) smt (\sn -> pipeStableMap.at sn .= Nothing)
    incrStackHeight -= 1

  checkSat = do
    smt <- get
    let cmd = "(check-sat)"
    when (smt^.isDebugging) $ liftIO $ ByteString.Char8.putStrLn $ toLazyByteString cmd
    result <- liftIO $ B.command (smt^.pipe) cmd
    when (smt^.isDebugging) $ liftIO $ ByteString.Char8.putStrLn result
    case parseOnly resultParser (toStrict result) of
      Left e    -> liftIO $ do
        print result
        error e
      Right res -> return res

  getModel = do
    smt   <- get
    let cmd = "(get-model)"
    when (smt^.isDebugging) $ liftIO $ ByteString.Char8.putStrLn $ toLazyByteString cmd
    model <- liftIO $ B.command (smt^.pipe) cmd
    when (smt^.isDebugging) $ liftIO $ ByteString.Char8.putStrLn model
    case parseOnly anyModelParser (toStrict model) of
      Left e    -> liftIO $ do
        print model
        error e
      Right sol -> return sol

  getValue :: forall t. KnownSMTSort t => Expr t -> m (Maybe (Decoded (Expr t)))
  getValue v@(Var x) = do
    smt   <- get
    let cmd = renderUnary "get-value" $ "(" <> render x <> ")"
    when (smt^.isDebugging) $ liftIO $ ByteString.Char8.putStrLn $ toLazyByteString cmd
    model <- liftIO $ B.command (smt^.pipe) cmd
    when (smt^.isDebugging) $ liftIO $ ByteString.Char8.putStrLn model
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
    smt <- get
    sExpr <- runSharing expr
    let cmd = render $ Minimize sExpr
    when (smt^.isDebugging) $ liftIO $ ByteString.Char8.putStrLn $ toLazyByteString cmd
    liftIO $ B.command_ (smt^.pipe) cmd
  {-# INLINEABLE minimize #-}

  maximize expr = do
    smt <- get
    sExpr <- runSharing expr
    let cmd = render $ Maximize sExpr
    when (smt^.isDebugging) $ liftIO $ ByteString.Char8.putStrLn $ toLazyByteString cmd
    liftIO $ B.command_ (smt^.pipe) cmd
  {-# INLINEABLE maximize #-}

  assertSoft expr w gid = do
    smt <- get
    sExpr <- runSharing expr
    let cmd = render $ SoftFormula sExpr w gid
    when (smt^.isDebugging) $ liftIO $ ByteString.Char8.putStrLn $ toLazyByteString cmd
    liftIO $ B.command_ (smt^.pipe) cmd
  {-# INLINEABLE assertSoft #-}
