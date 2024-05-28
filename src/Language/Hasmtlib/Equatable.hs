module Language.Hasmtlib.Equatable where

-- | Test two as on equality and answer with b
--   Usage:
--     x <- var @RealType
--     y <- var @RealType
--     assert $ y === x
class Equatable b a where
  (===) :: a -> a -> b
  (/==) :: a -> a -> b
infix 4 ===, /==

-- This is why we need the ugly b in class definition
instance Eq a => Equatable Bool a where
  (===) = (==)
  (/==) = (/=)
