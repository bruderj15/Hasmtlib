{-# LANGUAGE DefaultSignatures #-}

module Language.Hasmtlib.Variable where

import Language.Hasmtlib.Internal.Expr
import Language.Hasmtlib.Type.MonadSMT
import Language.Hasmtlib.Type.SMTSort
import Data.Proxy
import GHC.Generics

-- | Construct a variable datum of a data-type by creating variables for all its fields.
--
--   You can derive an instance of this class if your type is 'Generic' and has exactly one constructor.
--
-- @
--    data V3 a = V3 a a a deriving Generic
--    instance Variable a => V3 a
-- @
--
--    >>> varV3 :: V3 (Expr RealType) <- variable ; varV3
--        V3 (Expr RealType) (Expr RealType) (Expr RealType)
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

instance Variable a => Variable (Maybe a) where
  variable = Just <$> variable

instance Variable b => Variable (Either a b) where
  variable = Right <$> variable

class GVariable f where
  gvariable :: MonadSMT s m => m (f a)

instance GVariable U1 where
  gvariable = return U1

instance (GVariable f, GVariable g) => GVariable (f :*: g) where
  gvariable = liftA2 (:*:) gvariable gvariable

instance GVariable f => GVariable (M1 i c f) where
  gvariable = M1 <$> gvariable

instance Variable a => GVariable (K1 i a) where
  gvariable = K1 <$> variable
