{-# LANGUAGE AllowAmbiguousTypes #-}

module Language.Hasmtlib.Orderable where

import Language.Hasmtlib.Equatable

-- | Test two as on equality and answer with b
--   Use with type-applications or scoped-type-variables
--   Usage:
--     let x = 10
--         y = 5
--         b :: Bool = x <= y
class Equatable a b => Orderable a b where
  (<=?) :: a -> a -> b
  (>=?) :: a -> a -> b
  (<?)  :: a -> a -> b
  (>?)  :: a -> a -> b
  min'  :: a -> a -> a
  max'  :: a -> a -> a
infix 4 <?, <=?, >=?, >?

instance Ord a => Orderable a Bool where
  (<=?) = (<=)
  (>=?) = (>=)
  (<?)  = (<)
  (>?)  = (>)
  min'  = min
  max'  = max

infix 7 -?, +?
-- Bind ambiguous b to Bool where we know it has an instance Orderable
-- so the user can call: 'b = x -? y'
-- without the need to apply types
-- | Infix min
(-?) :: forall a. Orderable a Bool => a -> a -> a
(-?) = min' @a @Bool

-- | Infix max
(+?) :: forall a. Orderable a Bool => a -> a -> a
(+?) = max' @a @Bool
