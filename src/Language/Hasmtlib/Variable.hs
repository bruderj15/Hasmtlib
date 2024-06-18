{-# LANGUAGE DefaultSignatures #-}

module Language.Hasmtlib.Variable where

import Language.Hasmtlib.Internal.Expr
import Language.Hasmtlib.Type.MonadSMT
import Data.Proxy

-- | Construct a variable datum of a data-type by creating variables for all its fields.
-- 
-- @
--    data V3 a = V3 a a a
--    instance Variable a => V3 a 
-- @
--
--    >>> varV3 <- variable @(V3 (Expr RealType)) ; varV3
--        V3 (Var RealType) (Var RealType) (Var RealType)
class Variable a where
  variable :: MonadSMT s m => m a
  default variable :: (MonadSMT s m, Applicative f, Traversable f, Variable b, a ~ f b) => m a
  variable = sequenceA $ pure variable
  {-# INLINEABLE variable #-}

-- | Wrapper for 'variable' which takes a 'Proxy'
variable' :: forall s m a. (MonadSMT s m, Variable a) => Proxy a -> m a
variable' _ = variable @a
{-# INLINE variable' #-}

instance KnownSMTSort t => Variable (Expr t) where
  variable = var
  {-# INLINE variable #-}

instance Variable () where
  variable = return ()

instance (Variable a, Variable b) => Variable (a,b) where
  variable = (,) <$> variable <*> variable

instance (Variable a, Variable b, Variable c) => Variable (a,b,c) where
  variable = (,,) <$> variable <*> variable  <*> variable

instance (Variable a, Variable b, Variable c, Variable d) => Variable (a,b,c,d) where
  variable = (,,,) <$> variable <*> variable <*> variable <*> variable

instance (Variable a, Variable b, Variable c, Variable d, Variable e) => Variable (a,b,c,d,e) where
  variable = (,,,,) <$> variable <*> variable <*> variable <*> variable <*> variable

instance (Variable a, Variable b, Variable c, Variable d, Variable e, Variable f) => Variable (a,b,c,d,e,f) where
  variable = (,,,,,) <$> variable <*> variable <*> variable <*> variable <*> variable <*> variable

instance (Variable a, Variable b, Variable c, Variable d, Variable e, Variable f, Variable g) => Variable (a,b,c,d,e,f,g) where
  variable = (,,,,,,) <$> variable <*> variable <*> variable <*> variable <*> variable <*> variable <*> variable

instance (Variable a, Variable b, Variable c, Variable d, Variable e, Variable f, Variable g, Variable h) => Variable (a,b,c,d,e,f,g,h) where
  variable = (,,,,,,,) <$> variable <*> variable <*> variable <*> variable <*> variable <*> variable <*> variable <*> variable

instance Variable a => Variable (Maybe a)
instance Variable b => Variable (Either a b)
