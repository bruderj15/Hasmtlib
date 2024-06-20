{-# LANGUAGE TemplateHaskell #-}

module Language.Hasmtlib.Internal.ArrayMap where

import Data.Proxy
import qualified Data.Map as Map  
import Control.Lens
  
class ArrayMap f k v where
  asConst'   :: Proxy f -> Proxy k -> v -> f k v 
  viewConst  :: f k v -> v
  select     :: f k v -> k -> v
  store      :: f k v -> k -> v -> f k v

asConst :: forall f k v. ArrayMap f k v => v -> f k v
asConst = asConst' (Proxy @f) (Proxy @k)
  
data ConstArray k v = ConstArray 
  { _arrConst :: v
  , _stored :: Map.Map k v
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable)
$(makeLenses ''ConstArray)

instance Ord k => ArrayMap ConstArray k v where
  asConst' _ _ x = ConstArray x Map.empty
  viewConst arr = arr^.arrConst
  select arr i = case Map.lookup i (arr^.stored) of
    Nothing -> viewConst arr
    Just x  -> x
  store arr i x = arr & stored %~ Map.insert i x