{-# LANGUAGE AllowAmbiguousTypes #-}

module Language.Hasmtlib.Type.Expr
 ( SMTType(..)
 , SMTVar(..)
 , ValueType
 , Value(..), extractValue, putValue
 , Repr(..), KnownSMTRepr(..), SomeKnownSMTRepr(..)
 , Expr
 , for_all , exists
 , module Language.Hasmtlib.Internal.Expr.Num
 )
where

import Language.Hasmtlib.Internal.Expr
import Language.Hasmtlib.Internal.Expr.Num

-- | A universal quantification for any specific type
--   If the type cannot be inferred, apply a type-annotation.
--   Nested quantifiers are also supported.
-- 
--   Usage:
--   assert $
--      for_all @IntType $ \x ->
--         x + 0 === x && 0 + x === 0 
--    
--   The lambdas 'x' is all-quantified here.
--   It will only be scoped for the lambdas body.
for_all :: forall t. KnownSMTRepr t => (Expr t -> Expr BoolType) -> Expr BoolType
for_all = ForAll Nothing

-- | An existential quantification for any specific type
--   If the type cannot be inferred, apply a type-annotation.
--   Nested quantifiers are also supported.
-- 
--   Usage:
--   assert $
--      for_all @(BvType 8) $ \x ->
--          exists $ \y ->
--            x - y === 0 
--    
--   The lambdas 'y' is existentially quantified here.
--   It will only be scoped for the lambdas body.
exists :: forall t. KnownSMTRepr t => (Expr t -> Expr BoolType) -> Expr BoolType
exists = Exists Nothing