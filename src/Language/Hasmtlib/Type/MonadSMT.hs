module Language.Hasmtlib.Type.MonadSMT where

import Language.Hasmtlib.Internal.Expr
import Language.Hasmtlib.Type.Option
import Data.Proxy
import Control.Monad
import Control.Monad.State

class MonadState s m => MonadSMT s m where
  -- | Construct a variable.
  -- 
  -- @
  -- x :: SMTVar RealType <- smtvar' (Proxy @RealType)
  -- @
  -- 
  smtvar'    :: forall t. KnownSMTSort t => Proxy t -> m (SMTVar t)
  
  -- | Construct a variable as expression.
  -- 
  -- @
  -- x :: Expr RealType <- var' (Proxy @RealType)
  -- @
  -- 
  var'       :: forall t. KnownSMTSort t => Proxy t -> m (Expr t)

  -- | Assert a boolean expression.
  -- 
  -- @
  -- x :: Expr IntType <- var @IntType
  -- assert $ x + 5 === 42
  -- @
  -- 
  assert    :: Expr BoolSort -> m ()

  -- | Set an SMT-Solver-Option.
  -- 
  -- @
  -- setOption $ Incremental True
  -- @
  -- 
  setOption :: SMTOption -> m ()

  -- | Set the logic for the SMT-Solver to use.
  -- 
  -- @
  -- setLogic "QF_LRA"
  -- @
  -- 
  setLogic  :: String -> m ()

-- | Wrapper for 'var'' which hides the 'Proxy'.
var :: forall t s m. (KnownSMTSort t, MonadSMT s m) => m (Expr t)
var = var' (Proxy @t)
{-# INLINE var #-}

-- | Wrapper for 'smtvar'' which hides the 'Proxy'.
-- | This is mainly intended for internal use.
-- | In the API use 'var' instead.
smtvar :: forall t s m. (KnownSMTSort t, MonadSMT s m) => m (SMTVar t)
smtvar = smtvar' (Proxy @t)
{-# INLINE smtvar #-}

-- | Create a constant.
-- 
--   >>> constant True
--       Constant (BoolValue True)
--
--   >>> let x :: Integer = 10 ; constant x
--       Constant (IntValue 10)
--
--   >>> constant @IntType 5
--       Constant (IntValue 5)
--
--   >>> constant @(BvType 8) 5
--       Constant (BvValue 0000101)
constant :: KnownSMTSort t => HaskellType t -> Expr t
constant = Constant . wrapValue
{-# INLINE constant #-}

--   We need this separate so we get a pure API for quantifiers
--   Ideally we would do that when rendering the expression
--   However renderSMTLib2 is pure but we need a new quantified var which is stateful
-- | Assign quantified variables to all quantified subexpressions of an expression.
--   This shall only be used internally.
--   Usually before rendering an assert.
quantify :: MonadSMT s m => Expr t -> m (Expr t)
quantify (Not x)      = fmap   Not  (quantify x)
quantify (And x y)    = liftM2 And  (quantify x) (quantify y)
quantify (Or x y)     = liftM2 Or   (quantify x) (quantify y)
quantify (Impl x y)   = liftM2 Impl (quantify x) (quantify y)
quantify (Xor x y)    = liftM2 Xor  (quantify x) (quantify y)
quantify (Ite p t f)  = liftM3 Ite  (quantify p) (quantify t) (quantify f)
quantify (ForAll _ f) = do
  qVar <- smtvar
  qBody <- quantify $ f $ Var qVar
  return $ ForAll (Just qVar) (const qBody)
quantify (Exists _ f) = do
  qVar <- smtvar
  qBody <- quantify $ f $ Var qVar
  return $ Exists (Just qVar) (const qBody)
quantify expr = return expr