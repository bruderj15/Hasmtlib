{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Hasmtlib.Type.SMT where

import Language.Hasmtlib.Type.Expr
import Data.Default
import Data.AttoLisp
import Data.Sequence hiding ((|>), filter)
import Data.Data (Data, toConstr, showConstr)
import Control.Monad.State
import Control.Lens hiding (List)

data SMTOption =
    PrintSuccess  Bool          -- | Print \"success\" after each operation
  | ProduceModels Bool          -- | Produce a satisfying assignment after each successful checkSat
  deriving (Show, Eq, Ord, Data)

instance ToLisp SMTOption where
  toLisp (PrintSuccess  b) = List [Symbol "set-option", Symbol ":print-success",  Symbol $ if b then "true" else "false"]
  toLisp (ProduceModels b) = List [Symbol "set-option", Symbol ":produce-models", Symbol $ if b then "true" else "false"]

-- | SMT State
data SMT = SMT
  { _lastVarId :: {-# UNPACK #-} !Int             -- | Last Id assigned to a new var
  , _vars     :: Seq (SomeKnownSMTRepr SMTVar)    -- | All constructed variables
  , _formulas :: Seq (Expr BoolType)              -- | All asserted formulas
  , _mlogic   :: Maybe String                     -- | Logic for the SMT-Solver
  , _options  :: [SMTOption]                      -- | All manually configured SMT-Solver-Options
  }

$(makeLenses ''SMT)

instance Default SMT where
  def = SMT 0 mempty mempty Nothing mempty

-- | Set the logic for the SMT-Solver to use
--   Usage:
--      setLogic "QF_LRA"
setLogic :: MonadState SMT m => String -> m ()
setLogic l = mlogic ?= l

-- | Set an SMT-Solver-Option
setOption :: MonadState SMT m => SMTOption -> m ()
setOption opt = options %= ((opt:) . filter (not . eqCon opt))
  where
    eqCon :: SMTOption -> SMTOption -> Bool
    eqCon l r = showConstr (toConstr l) == showConstr (toConstr r)

-- | Construct a variable
--   Usage:
--      x :: Expr RealType <- var @RealType
var :: forall t m. (KnownSMTRepr t, MonadState SMT m) => m (Expr t)
var = do
  smt <- get
  let la' = smt^.lastVarId + 1
      newVar = SMTVar la' Nothing
  modify $ \s -> s & vars %~ (|> SomeKnownSMTRepr newVar) & lastVarId %~ (+1)
  return $ Var newVar

-- | Assert a boolean expression
--   Usage
--      x :: Expr IntType <- var @IntType
--      assert $ x + 5 === 42
assert :: MonadState SMT m => Expr BoolType -> m ()
assert expr = modify $ \s -> s & formulas %~ (|> expr)

checkSat :: MonadState SMT m => m ()
checkSat = _