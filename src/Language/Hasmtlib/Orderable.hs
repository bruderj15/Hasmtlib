{-# LANGUAGE AllowAmbiguousTypes #-}

module Language.Hasmtlib.Orderable where

import Language.Hasmtlib.Equatable
import Language.Hasmtlib.Boolean

-- | Compare two as and answer with b
--   Ambiguous @min'@ an @max'@ require type application for b.
--   Usage:
--     x <- var @RealType
--     y <- var @RealType
--     b <- var @BoolType
--     assert $ x >? y
--     assert $ x === min' @(Expr BoolType) 42 100
class (Boolean b, Equatable b a) => Orderable b a where
  (<=?) :: a -> a -> b
  
  (>=?) :: a -> a -> b
  x >=? y = y <=? x
  
  (<?)  :: a -> a -> b
  x <? y = not' $ y <=? x

  (>?)  :: a -> a -> b
  x >? y = not' $ x <=? y

  min'  :: a -> a -> a

  max'  :: a -> a -> a
  
  {-# MINIMAL (<=?), min', max' #-}
infix 4 <?, <=?, >=?, >?

-- This is why we need the ugly b in class definition
instance Ord a => Orderable Bool a where
  (<=?) = (<=)
  (>=?) = (>=)
  (<?)  = (<)
  (>?)  = (>)
  min'  = min
  max'  = max
