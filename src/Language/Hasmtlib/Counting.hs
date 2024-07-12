module Language.Hasmtlib.Counting where

import Prelude hiding (not, (&&), (||), or)
import Language.Hasmtlib.Internal.Expr.Num ()
import Language.Hasmtlib.Internal.Expr
import Language.Hasmtlib.Equatable
import Language.Hasmtlib.Orderable
import Language.Hasmtlib.Iteable

-- | Out of many bool-expressions build a formula which encodes that __at most__ 'k' of them are 'true'
atMost  :: forall t f. (Functor f, Foldable f, Num (Expr t), Orderable (Expr t)) => Expr t -> f (Expr BoolSort) -> Expr BoolSort
atMost  k = (<=? k) . sum . fmap (\b -> ite b 1 0)
{-# INLINEABLE atMost #-}

-- | Out of many bool-expressions build a formula which encodes that __at least__ 'k' of them are 'true'
atLeast :: forall t f. (Functor f, Foldable f, Num (Expr t), Orderable (Expr t)) => Expr t -> f (Expr BoolSort) -> Expr BoolSort
atLeast k = (>=? k) . sum . fmap (\b -> ite b 1 0)
{-# INLINEABLE atLeast #-}

-- | Out of many bool-expressions build a formula which encodes that __exactly__ 'k' of them are 'true'
exactly :: forall t f. (Functor f, Foldable f, Num (Expr t), Orderable (Expr t)) => Expr t -> f (Expr BoolSort) -> Expr BoolSort
exactly k = (=== k) . sum . fmap (\b -> ite b 1 0)
{-# INLINEABLE exactly #-}
