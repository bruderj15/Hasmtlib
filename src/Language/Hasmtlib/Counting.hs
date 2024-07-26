module Language.Hasmtlib.Counting where

import Prelude hiding (not, (&&), (||), or)
import Language.Hasmtlib.Internal.Expr.Num ()
import Language.Hasmtlib.Internal.Expr
import Language.Hasmtlib.Type.SMTSort
import Language.Hasmtlib.Equatable
import Language.Hasmtlib.Orderable
import Language.Hasmtlib.Iteable
import Data.Proxy

-- | Wrapper for 'count' which takes a 'Proxy'.
count' :: forall t f. (Functor f, Foldable f, Num (Expr t)) => Proxy t -> f (Expr BoolSort) -> Expr t
count' _ = sum . fmap (\b -> ite b 1 0)
{-# INLINEABLE count' #-}

-- | Out of many bool-expressions build a formula which encodes how many of them are 'true'.
count :: forall t f. (Functor f, Foldable f, Num (Expr t)) => f (Expr BoolSort) -> Expr t
count = count' (Proxy @t)
{-# INLINE count #-}

-- | Out of many bool-expressions build a formula which encodes that __at most__ 'k' of them are 'true'.
atMost  :: forall t f. (Functor f, Foldable f, Num (Expr t), Orderable (Expr t)) => Expr t -> f (Expr BoolSort) -> Expr BoolSort
atMost  k = (<=? k) . count
{-# INLINEABLE atMost #-}

-- | Out of many bool-expressions build a formula which encodes that __at least__ 'k' of them are 'true'.
atLeast :: forall t f. (Functor f, Foldable f, Num (Expr t), Orderable (Expr t)) => Expr t -> f (Expr BoolSort) -> Expr BoolSort
atLeast k = (>=? k) . count
{-# INLINEABLE atLeast #-}

-- | Out of many bool-expressions build a formula which encodes that __exactly__ 'k' of them are 'true'.
exactly :: forall t f. (Functor f, Foldable f, Num (Expr t), Orderable (Expr t)) => Expr t -> f (Expr BoolSort) -> Expr BoolSort
exactly k = (=== k) . count
{-# INLINEABLE exactly #-}
