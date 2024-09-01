{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
This module provides a class 'ArrayMap' and a concrete implementation with 'ConstArray' for
McCarthy's basic array theory.
-}
module Language.Hasmtlib.Type.ArrayMap
(
  -- * Class
  ArrayMap(..)
, asConst

  -- * Type
, ConstArray(..)

  -- * Lens
, arrConst, stored
)
where

import Data.Proxy
import qualified Data.Map as Map
import Control.Lens

-- | Class that allows access to a map-like array where any value is either the default value or an overwritten values.
--   Every index has a value by default.
--   Values at indices can be overwritten manually.
--
--   Based on McCarthy`s basic array theory.
--
--   Therefore the following axioms must hold:
--
-- 1. forall A i x: arrSelect (arrStore i x) == x
--
-- 2. forall A i j x: i /= j ==> (arrSelect (arrStore i x) j === arrSelect A j)
class ArrayMap f k v where
  -- | Construct an 'ArrayMap' via it's const value.
  asConst'   :: Proxy f -> Proxy k -> v -> f k v

  -- | View the const value of the 'ArrayMap'.
  viewConst  :: f k v -> v

  -- | Select a value from the 'ArrayMap'.
  --
  -- Returns the specific value for given key if there is one. Returns the const value otherwise.
  arrSelect :: f k v -> k -> v

  -- | Store a specific value at a given key in an 'ArrayMap'.
  arrStore :: f k v -> k -> v -> f k v

-- | Wrapper for 'asConst'' which hides the 'Proxy'.
asConst :: forall f k v. ArrayMap f k v => v -> f k v
asConst = asConst' (Proxy @f) (Proxy @k)

-- | A map-like array with a default constant value and partially overwritten values.
data ConstArray k v = ConstArray
  { _arrConst :: v
  , _stored :: Map.Map k v
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable)
$(makeLenses ''ConstArray)

instance Ord k => ArrayMap ConstArray k v where
  asConst' _ _ x = ConstArray x Map.empty
  viewConst arr = arr^.arrConst
  arrSelect arr i = case Map.lookup i (arr^.stored) of
    Nothing -> viewConst arr
    Just x  -> x
  arrStore arr i x = arr & stored %~ Map.insert i x
