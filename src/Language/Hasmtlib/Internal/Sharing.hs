{-# LANGUAGE Trustworthy #-}

module Language.Hasmtlib.Internal.Sharing
  ( Sharing(..)
  , SharingMode(..)
  , runSharing, share
  )
where

import Language.Hasmtlib.Internal.Uniplate1
import Language.Hasmtlib.Type.MonadSMT
import Language.Hasmtlib.Type.SMTSort
import Language.Hasmtlib.Type.Expr
import Data.GADT.Compare
import Data.HashMap.Lazy
import Data.Default
import Data.Kind
import Control.Monad.State
import Control.Lens
import System.Mem.StableName
import System.IO.Unsafe
import Unsafe.Coerce

-- | Mode used for sharing.
data SharingMode =
    None            -- ^ Common expressions are not shared at all
  | StableNames     -- ^ Expressions that resolve to the same 'StableName' are shared
  deriving Show

instance Default SharingMode where
  def = StableNames

-- | States that can share expressions by comparing their 'StableName's.
class Sharing s where
  -- | A constraint on the monad used when asserting the shared node in 'assertSharedNode'.
  type SharingMonad s :: (Type -> Type) -> Constraint

  -- | A 'Lens'' on a mapping between a 'StableName' and it's 'Expr' we may share.
  stableMap :: Lens' s (HashMap (StableName ()) (SomeKnownSMTSort Expr))

  -- | Asserts that a node-expression is represented by it's auxiliary node-variable: @nodeExpr :: Expr t === nodeVar@.
  --   Also gives access to the 'StableName' of the original expression.
  assertSharedNode :: (MonadState s m, SharingMonad s m) => StableName () -> Expr BoolSort -> m ()

  -- | Sets the mode used for sharing common expressions. Defaults to 'StableNames'.
  setSharingMode :: MonadState s m => SharingMode -> m ()

-- | Shares all possible sub-expressions in given expression.
--   Replaces each node in the expression-tree with an auxiliary variable.
--   All nodes @x@ @y@ where @makeStableName x == makeStableName y@ are replaced with the same auxiliary variable.
--   Therefore this creates a DAG.
runSharing :: (KnownSMTSort t, MonadSMT s m, Sharing s, SharingMonad s m) => SharingMode -> Expr t -> m (Expr t)
runSharing None = return
runSharing StableNames = lazyParaM1 (
    \origExpr expr ->
      if isLeaf origExpr
      then return origExpr
      else case sortSing' origExpr of   -- scopes Equatable (Expr t) for specific t
        SBoolSort      -> share origExpr expr
        SIntSort       -> share origExpr expr
        SRealSort      -> share origExpr expr
        SBvSort _ _    -> share origExpr expr
        SArraySort _ _ -> share origExpr expr
        SStringSort    -> share origExpr expr)

-- | Returns an auxiliary variable representing this expression node.
--   If such a shared auxiliary variable exists already, returns that.
--   Otherwise creates one and returns it.
share :: (Equatable (Expr t), KnownSMTSort t, MonadSMT s m, Sharing s, SharingMonad s m) => Expr t -> m (Expr t) -> m (Expr t)
share expr@(ForAll _ _) _ = return expr     -- sharing quantified expression would out-scope quantified var
share expr@(Exists _ _) _ = return expr
share origExpr expr = do
  let sn = unsafePerformIO (makeStableName' origExpr)
   in use (stableMap.at sn) >>= \mexpr' -> case mexpr' of
        Just (SomeSMTSort expr') -> case geq (sortSing' origExpr) (sortSing' expr') of
          Nothing -> expr >>= makeNode sn
          Just Refl -> return expr'
        Nothing -> expr >>= makeNode sn

makeNode :: (Equatable (Expr t), KnownSMTSort t, MonadSMT s m, Sharing s, SharingMonad s m) => StableName () -> Expr t -> m (Expr t)
makeNode sn nodeExpr = do
  nodeVar <- var
  assertSharedNode sn $ nodeVar === nodeExpr
  stableMap.at sn ?= SomeSMTSort nodeVar
  return nodeVar

makeStableName' :: a -> IO (StableName ())
makeStableName' x = x `seq` fmap unsafeCoerce (makeStableName x)
