{- |
This module provides functions for counting symbolic formulas and creating cardinality-constraints.

Internally this converts each given 'Expr' 'BoolSort' into a numerical 'Expr' using 'ite', then sums them up:

@
  count :: forall t f. (Functor f, Foldable f, Num (Expr t)) => f (Expr BoolSort) -> Expr t
  count = sum . fmap (\\b -> ite b 1 0)
@

Therefore additional information for the temporal summation may need to be provided.

E.g. if your logic is \"QF_LIA\" you would want @'count'' \@'IntSort' $ ...@

It is worth noting that some cardinality constraints use optimized encodings for special cases.
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
, amoSqrt
, amoQuad
)
where

import Prelude hiding (not, (&&), (||), and, or, all, any)
import Language.Hasmtlib.Type.SMTSort
import Language.Hasmtlib.Type.Expr
import Language.Hasmtlib.Boolean
import Data.Foldable (toList)
import Data.List (transpose)
import Data.Proxy
import Control.Lens

-- | Wrapper for 'count' which takes a 'Proxy'.
count' :: forall t f. (Functor f, Foldable f, Num (Expr t)) => Proxy t -> f (Expr BoolSort) -> Expr t
count' _ = sum . fmap (\b -> ite b 1 0)
{-# INLINE count' #-}

-- | Out of many bool-expressions build a formula which encodes how many of them are 'true'.
count :: forall t f. (Functor f, Foldable f, Num (Expr t)) => f (Expr BoolSort) -> Expr t
count = count' (Proxy @t)
{-# INLINE count #-}

-- | Out of many bool-expressions build a formula which encodes that __at most__ @k@ of them are 'true'.
--
--   'atMost' is defined as follows:
--
-- @
-- 'atMost' 0 = 'nand'
-- 'atMost' 1 = 'amoSqrt'
-- 'atMost' k = ('<=?' k) . 'count'
-- @
atMost  :: forall t f. (Functor f, Foldable f, Num (Expr t), Orderable (Expr t)) => Expr t -> f (Expr BoolSort) -> Expr BoolSort
atMost 0 = nand
atMost 1 = amoSqrt
atMost k = (<=? k) . count
{-# INLINE atMost #-}

-- | Out of many bool-expressions build a formula which encodes that __at least__ @k@ of them are 'true'.
--
--   'atLeast' is defined as follows:
--
-- @
-- 'atLeast' 0 = 'const' 'true'
-- 'atLeast' 1 = 'or'
-- 'atLeast' k = ('>=?' k) . 'count'
-- @
atLeast :: forall t f. (Functor f, Foldable f, Num (Expr t), Orderable (Expr t)) => Expr t -> f (Expr BoolSort) -> Expr BoolSort
atLeast 0 = const true
atLeast 1 = or
atLeast k = (>=? k) . count
{-# INLINE atLeast #-}

-- | Out of many bool-expressions build a formula which encodes that __exactly__ @k@ of them are 'true'.
--
--   'exactly' is defined as follows:
--
-- @
-- 'exactly' 0 xs = 'nand' xs
-- 'exactly' 1 xs = 'atLeast' \@t 1 xs '&&' 'atMost' \@t 1 xs
-- 'exactly' k xs = 'count' xs '===' k
-- @
exactly :: forall t f. (Functor f, Foldable f, Num (Expr t), Orderable (Expr t)) => Expr t -> f (Expr BoolSort) -> Expr BoolSort
exactly 0 xs = nand xs
exactly 1 xs = atLeast @t 1 xs && atMost @t 1 xs
exactly k xs = count xs === k
{-# INLINE exactly #-}

-- | The squareroot-encoding, also called product-encoding, is a special encoding for @atMost 1@.
--
--   The original product-encoding provided by /Jingchao Chen/ in /A New SAT Encoding of the At-Most-One Constraint (2010)/
--   used auxiliary variables and would therefore be monadic.
--   It requires \( 2 \sqrt{n} + \mathcal{O}(\sqrt[4]{n}) \) auxiliary variables and
--   \( 2n + 4\sqrt{n} + \mathcal{O}(\sqrt[4]{n}) \) clauses.
--
--   To make this encoding pure, all auxiliary variables are replaced with a disjunction of size \( \mathcal{O}(\sqrt{n}) \).
--   Therefore zero auxiliary variables are required and technically clause-count remains the same, although the clauses get bigger.
amoSqrt :: (Foldable f, Boolean b) => f b -> b
amoSqrt xs
  | length xs < 10 = amoQuad $ toList xs
  | otherwise =
      let n = toInteger $ length xs
          p = ceiling $ sqrt $ fromInteger n
          rows = splitEvery (fromInteger p) $ toList xs
          columns = transpose rows
          vs = or <$> rows
          us = or <$> columns
       in amoSqrt vs && amoSqrt us &&
          and (imap (\j r -> and $ imap (\i x -> (x ==> us !! i) && (x ==> vs !! j)) r) rows)
  where
    splitEvery n = takeWhile (not . null) . map (take n) . iterate (drop n)

-- | The quadratic-encoding, also called pairwise-encoding, is a special encoding for @atMost 1@.
--
--   It's the naive encoding for the at-most-one-constraint and produces \( \binom{n}{2} \) clauses and no auxiliary variables..
amoQuad :: Boolean b => [b] -> b
amoQuad as = and $ do
  ys <- subs 2 as
  return $ any not ys
  where
    subs :: Int -> [a] -> [[a]]
    subs 0 _ = [[]]
    subs _ [] = []
    subs k (x : xs) = map (x :) (subs (k -1) xs) <> subs k xs
{-# INLINE amoQuad #-}
