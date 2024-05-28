module Language.Hasmtlib.Equatable where

-- | Test two as on equality and answer with b
--   Use with type-applications or scoped-type-variables
--   Usage:
--     let x = 10
--         y = 5
--         b :: Bool = x /== y
class Equatable a b where
  (===) :: a -> a -> b
  (/==) :: a -> a -> b
infix 4 ===, /==

instance Eq a => Equatable a Bool where
  (===) = (==)
  (/==) = (/=)
