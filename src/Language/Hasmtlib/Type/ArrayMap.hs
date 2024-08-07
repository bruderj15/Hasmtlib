{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Hasmtlib.Type.ArrayMap where

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
-- 1. forall A i x: arrSelect (store A i x) == x
--
-- 2. forall A i j x: i /= j ==> (arrSelect (arrStore A i x) j === arrSelect A j)
class ArrayMap f k v where
  asConst'   :: Proxy f -> Proxy k -> v -> f k v
  viewConst  :: f k v -> v
  arrSelect  :: f k v -> k -> v
  arrStore   :: f k v -> k -> v -> f k v

-- | Wrapper for 'asConst'' which hides the 'Proxy'
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
