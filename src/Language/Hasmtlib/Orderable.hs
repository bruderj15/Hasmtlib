{-# LANGUAGE AllowAmbiguousTypes #-}

module Language.Hasmtlib.Orderable where

import Language.Hasmtlib.Equatable

-- | Compare two as and answer with b
--   Ambiguous @min'@ an @max'@ require type application for b.
--   Usage:
--     x <- var @RealType
--     y <- var @RealType
--     b <- var @BoolType
--     assert $ x >? y
--     assert $ x === min' @(Expr BoolType) 42 100
class Equatable b a => Orderable b a where
  (<=?) :: a -> a -> b
  (>=?) :: a -> a -> b
  (<?)  :: a -> a -> b
  (>?)  :: a -> a -> b
  min'  :: a -> a -> a
  max'  :: a -> a -> a
infix 4 <?, <=?, >=?, >?

-- This is why we need the ugly b in class definition
instance Ord a => Orderable Bool a where
  (<=?) = (<=)
  (>=?) = (>=)
  (<?)  = (<)
  (>?)  = (>)
  min'  = min
  max'  = max
