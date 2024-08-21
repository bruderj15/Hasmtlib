{-# LANGUAGE FunctionalDependencies #-}

module Language.Hasmtlib.Internal.Uniplate1 where

import Language.Hasmtlib.Internal.Constraint
import Data.Kind

type Uniplate1 :: (k -> Type) -> [k -> Constraint] -> Constraint
class Uniplate1 f cs | f -> cs where
  uniplate1 :: (Applicative m, AllC cs b) => (forall a. AllC cs a => f a -> m (f a)) -> f b -> m (f b)

transformM1 :: (Monad m, Uniplate1 f cs, AllC cs b) => (forall a. AllC cs a => f a -> m (f a)) -> f b -> m (f b)
transformM1 f x = uniplate1 (transformM1 f) x >>= f

lazyParaM1 :: (Monad m, Uniplate1 f cs, AllC cs b) => (forall a. AllC cs a => f a -> m (f a) -> m (f a)) -> f b -> m (f b)
lazyParaM1 f x = f x (uniplate1 (lazyParaM1 f) x)
