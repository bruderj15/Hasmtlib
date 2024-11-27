{-# LANGUAGE FunctionalDependencies #-}

module Language.Hasmtlib.Internal.Uniplate1 where

import Data.Functor.Identity
import Data.Some.Constraint
import Data.Kind

type Uniplate1 :: (k -> Type) -> [k -> Constraint] -> Constraint
class Uniplate1 f cs | f -> cs where
  uniplate1 :: (Applicative m, AllC cs b) => (forall a. AllC cs a => f a -> m (f a)) -> f b -> m (f b)

transformM1 :: (Monad m, Uniplate1 f cs, AllC cs b) => (forall a. AllC cs a => f a -> m (f a)) -> f b -> m (f b)
transformM1 f x = uniplate1 (transformM1 f) x >>= f

transform1 :: (Uniplate1 f cs, AllC cs b) => (forall a. AllC cs a => f a -> f a) -> f b -> f b
transform1 f = runIdentity . transformM1 (Identity . f)
{-# INLINE transform1 #-}

lazyParaM1 :: (Monad m, Uniplate1 f cs, AllC cs b) => (forall a. AllC cs a => f a -> m (f a) -> m (f a)) -> f b -> m (f b)
lazyParaM1 f x = f x (uniplate1 (lazyParaM1 f) x)
