module Language.Hasmtlib.Internal.Constraint where

import Data.Kind

-- | AllC ensures that a list of constraints is applied to a poly-kinded 'Type' k
--
-- @
-- AllC '[]       k = ()
-- AllC (c ': cs) k = (c k, AllC cs k)
-- @
type AllC :: [k -> Constraint] -> k -> Constraint
type family AllC cs k :: Constraint where
  AllC '[]       k = ()
  AllC (c ': cs) k = (c k, AllC cs k)
