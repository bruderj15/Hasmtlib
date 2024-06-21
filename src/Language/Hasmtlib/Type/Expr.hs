{-# LANGUAGE AllowAmbiguousTypes #-}

module Language.Hasmtlib.Type.Expr
 ( SMTSort(..)
 , SMTVar(..), varId
 , HaskellType
 , Value(..), unwrapValue, wrapValue
 , SSMTSort(..), KnownSMTSort(..), sortSing', SomeSMTSort(..), SomeKnownSMTSort
 , Expr
 , for_all , exists
 , select, store
 , module Language.Hasmtlib.Internal.Expr.Num
 )
where

import Language.Hasmtlib.Internal.Expr
import Language.Hasmtlib.Internal.Expr.Num

-- | A universal quantification for any specific 'SMTSort'.
--   If the type cannot be inferred, apply a type-annotation.
--   Nested quantifiers are also supported.
-- 
--   Usage:
--
--   @
--   assert $
--      for_all @IntSort $ \x ->
--         x + 0 === x && 0 + x === x 
--   @   
-- 
--   The lambdas 'x' is all-quantified here.
--   It will only be scoped for the lambdas body.
for_all :: forall t. KnownSMTSort t => (Expr t -> Expr BoolSort) -> Expr BoolSort
for_all = ForAll Nothing

-- | An existential quantification for any specific 'SMTSort'
--   If the type cannot be inferred, apply a type-annotation.
--   Nested quantifiers are also supported.
-- 
--   Usage:
-- 
--   @
--   assert $
--      for_all @(BvSort 8) $ \x ->
--          exists $ \y ->
--            x - y === 0 
--   @   
-- 
--   The lambdas 'y' is existentially quantified here.
--   It will only be scoped for the lambdas body.
exists :: forall t. KnownSMTSort t => (Expr t -> Expr BoolSort) -> Expr BoolSort
exists = Exists Nothing

-- | Select a value from an array.
select :: (KnownSMTSort k, KnownSMTSort v, Ord (HaskellType k)) => Expr (ArraySort k v) -> Expr k -> Expr v
select = ArrSelect

-- | Store a value in an array.
store :: (KnownSMTSort k, KnownSMTSort v, Ord (HaskellType k)) => Expr (ArraySort k v) -> Expr k -> Expr v -> Expr (ArraySort k v)
store = ArrStore