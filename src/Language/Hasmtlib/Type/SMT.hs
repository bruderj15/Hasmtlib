{-# LANGUAGE TemplateHaskell #-}

module Language.Hasmtlib.Type.SMT
 ( SMT, lastVarId, vars, formulas, mlogic, options
 , setLogic, SMTOption(..) 
-- setOption 
-- Currently do not export because smtlib-backends handles it vaguely: 
-- https://github.com/tweag/smtlib-backends/issues/70
 , var, constant
 , assert
 )
 where

import Language.Hasmtlib.Internal.Expr
import Data.Default
import Data.Coerce
import Data.Sequence hiding ((|>), filter)
import Data.Data (Data, toConstr, showConstr)
import Control.Monad.State
import Control.Lens hiding (List)

data SMTOption =
    PrintSuccess  Bool          -- | Print \"success\" after each operation
  | ProduceModels Bool          -- | Produce a satisfying assignment after each successful checkSat
  deriving (Show, Eq, Ord, Data)

-- | SMT State
data SMT = SMT
  { _lastVarId :: {-# UNPACK #-} !Int             -- | Last Id assigned to a new var
  , _vars     :: Seq (SomeKnownSMTRepr SMTVar)    -- | All constructed variables
  , _formulas :: Seq (Expr BoolType)              -- | All asserted formulas
  , _mlogic   :: Maybe String                     -- | Logic for the SMT-Solver
  , _options  :: [SMTOption]                      -- | All manually configured SMT-Solver-Options
  }

instance Default SMT where
  def = SMT 0 mempty mempty mempty mempty

$(makeLenses ''SMT)

-- | Set the logic for the SMT-Solver to use.
--   Usage:
--      setLogic "QF_LRA"
setLogic :: MonadState SMT m => String -> m ()
setLogic l = mlogic ?= l

-- | Set an SMT-Solver-Option.
setOption :: MonadState SMT m => SMTOption -> m ()
setOption opt = options %= ((opt:) . filter (not . eqCon opt))
  where
    eqCon :: SMTOption -> SMTOption -> Bool
    eqCon l r = showConstr (toConstr l) == showConstr (toConstr r)

-- | Construct a variable.
--   Usage:
--      x :: Expr RealType <- var @RealType
var :: forall t m. (KnownSMTRepr t, MonadState SMT m) => m (Expr t)
var = do
  smt <- get
  let la' = smt^.lastVarId + 1
      newVar = coerce la'
  modify $ \s -> s & vars %~ (|> SomeKnownSMTRepr newVar) & lastVarId %~ (+1)
  return $ Var newVar

-- | Create a constant.
--   This may be used to create constants from parameters.
--   Otherwise usage of overloaded numbers (fromInteger) should be preferred, if type can be inferred. 
--   Usage
--      >>> constant True
--          Constant (BoolValue True) 
-- 
--      >>> let x :: Integer = 10 ; constant x
--          Constant (IntValue 10)
-- 
--      >>> constant @IntType 5
--          Constant (IntValue 5)
-- 
--      >>> constant @(BvType 8) 5
--          Constant (BvValue 0000101)
constant :: KnownSMTRepr t => ValueType t -> Expr t
constant = Constant . putValue 

-- | Assert a boolean expression.
--   Usage
--      x :: Expr IntType <- var @IntType
--      assert $ x + 5 === 42
assert :: MonadState SMT m => Expr BoolType -> m ()
assert expr = modify $ \s -> s & formulas %~ (|> expr)
