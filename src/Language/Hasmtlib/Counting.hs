{- |
This module provides functions for counting symbolic formulas and creating cardinality-constraints.

Internally this converts each given 'Expr' 'BoolSort' into a numerical 'Expr' using 'ite', then sums them up:

@
  count :: forall t f. (Functor f, Foldable f, Num (Expr t)) => f (Expr BoolSort) -> Expr t
  count = sum . fmap (\\b -> ite b 1 0)
@

Therefore additional information for the temporal summation may need to be provided.

E.g. if your logic is \"QF_LIA\" you would want @'count'' \@'IntSort' $ ...@

It is worth noting that some cardinality constraints use optimized encodings, such as @'atLeast' 1 ≡ 'or'@.
-}
module Language.Hasmtlib.Counting
(
  -- * Count
  count, count'

  -- * At-Least
, atLeast

  -- * Exactly
, exactly

  -- * At-Most
, atMost
)
where

import Prelude hiding (not, (&&), (||), or)
import Language.Hasmtlib.Type.SMTSort
import Language.Hasmtlib.Type.Expr
import Language.Hasmtlib.Boolean
import Data.Proxy

-- | Wrapper for 'count' which takes a 'Proxy'.
count' :: forall t f. (Functor f, Foldable f, Num (Expr t)) => Proxy t -> f (Expr BoolSort) -> Expr t
count' _ = sum . fmap (\b -> ite b 1 0)
{-# INLINEABLE count' #-}

-- | Out of many bool-expressions build a formula which encodes how many of them are 'true'.
count :: forall t f. (Functor f, Foldable f, Num (Expr t)) => f (Expr BoolSort) -> Expr t
count = count' (Proxy @t)
{-# INLINE count #-}

-- | Out of many bool-expressions build a formula which encodes that __at most__ @k@ of them are 'true'.
atMost  :: forall t f. (Functor f, Foldable f, KnownSMTSort t, Num (HaskellType t), Ord (HaskellType t)) => Expr t -> f (Expr BoolSort) -> Expr BoolSort
atMost (Constant 0) = nand
atMost (Constant 1) = atMostOneLinear
atMost k = (<=? k) . count
{-# INLINE atMost #-}

atMostOneLinear :: (Foldable f, Boolean b) => f b -> b
atMostOneLinear xs =
  let (_, sz) = foldr (plus . (, false)) (false, false) xs
   in not sz
  where
    plus (xe, xz) (ye, yz) = (xe || ye, xz || yz || (xe && ye))

-- | Out of many bool-expressions build a formula which encodes that __at least__ @k@ of them are 'true'.
atLeast :: forall t f. (Functor f, Foldable f, KnownSMTSort t, Num (HaskellType t), Ord (HaskellType t)) => Expr t -> f (Expr BoolSort) -> Expr BoolSort
atLeast (Constant 0) = const true
atLeast (Constant 1) = or
atLeast k = (>=? k) . count
{-# INLINE atLeast #-}

-- | Out of many bool-expressions build a formula which encodes that __exactly__ @k@ of them are 'true'.
exactly :: forall t f. (Functor f, Foldable f, KnownSMTSort t, Num (HaskellType t), Ord (HaskellType t)) => Expr t -> f (Expr BoolSort) -> Expr BoolSort
exactly (Constant 0) = nand
exactly k = (=== k) . count
{-# INLINE exactly #-}
