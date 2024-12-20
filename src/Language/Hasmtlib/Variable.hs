{-# LANGUAGE DefaultSignatures #-}

{- |
This module provides the class 'Variable' which lets you create symbolical values of a data-type.

A generic default implementation with 'GVariable' is possible.

==== __Example__

@
data V3 a = V3 a a a deriving Generic
instance Codec a => Codec (V3 a)
instance Equatable a => Codec (V3 a)

problem :: MonadSMT s m => StateT s m (V3 (Expr RealSort))
problem = do
  let constantV3 = encode $ V3 7 69 42
  symbolicV3 <- variable \@(V3 (Expr RealSort))
  assert $ symbolicV3 /== constantV3
  return symbolicV3
@
-}
module Language.Hasmtlib.Variable
(
  -- * Class
  Variable(..)

  -- * Other functions
, variable'

  -- * Generics
, GVariable(..)
)
where

import Language.Hasmtlib.Type.Expr
import Language.Hasmtlib.Type.MonadSMT
import Language.Hasmtlib.Type.SMTSort
import Data.Proxy
import Data.Monoid (Sum, Product, First, Last, Alt, Dual)
import Data.Functor.Const (Const)
import Data.Functor.Identity (Identity)
import Data.Functor.Compose (Compose)
import GHC.Generics

-- | Construct a variable datum of a data-type by creating variables for all its fields.
--
--   You can derive an instance of this class if your type is 'Generic' and has exactly one constructor.
class Variable a where
  variable :: MonadSMT s m => m a
  default variable :: (MonadSMT s m, Generic a, GVariable (Rep a)) => m a
  variable = to <$> gvariable

-- | Wrapper for 'variable' which takes a 'Proxy'
variable' :: forall s m a. (MonadSMT s m, Variable a) => Proxy a -> m a
variable' _ = variable @a
{-# INLINE variable' #-}

instance KnownSMTSort t => Variable (Expr t) where
  variable = var
  {-# INLINE variable #-}

instance Variable ()
instance (Variable a, Variable b) => Variable (a,b)
instance (Variable a, Variable b, Variable c) => Variable (a,b,c)
instance (Variable a, Variable b, Variable c, Variable d) => Variable (a,b,c,d)
instance (Variable a, Variable b, Variable c, Variable d, Variable e) => Variable (a,b,c,d,e)
instance (Variable a, Variable b, Variable c, Variable d, Variable e, Variable f) => Variable (a,b,c,d,e,f)
instance (Variable a, Variable b, Variable c, Variable d, Variable e, Variable f, Variable g) => Variable (a,b,c,d,e,f,g)
instance (Variable a, Variable b, Variable c, Variable d, Variable e, Variable f, Variable g, Variable h) => Variable (a,b,c,d,e,f,g,h)
instance Variable a => Variable (Sum a)
instance Variable a => Variable (Product a)
instance Variable a => Variable (First a)
instance Variable a => Variable (Last a)
instance Variable a => Variable (Dual a)
instance Variable a => Variable (Identity a)
instance Variable m => Variable (Const m a)
instance Variable (f a) => Variable (Alt f a)
instance Variable (f (g a)) => Variable (Compose f g a)

instance Variable a => Variable (Maybe a) where
  variable = Just <$> variable

instance Variable b => Variable (Either a b) where
  variable = Right <$> variable

class GVariable f where
  gvariable :: MonadSMT s m => m (f a)

instance GVariable U1 where
  gvariable = return U1

instance (GVariable f, GVariable g) => GVariable (f :*: g) where
  gvariable = (:*:) <$> gvariable <*> gvariable

instance GVariable f => GVariable (M1 i c f) where
  gvariable = M1 <$> gvariable

instance Variable a => GVariable (K1 i a) where
  gvariable = K1 <$> variable
