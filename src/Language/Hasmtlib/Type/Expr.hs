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
import Data.Proxy
import GHC.TypeLits

for_all :: forall t s. (KnownSMTRepr t, KnownSymbol s) => (Expr t -> Expr BoolType) -> Expr BoolType
for_all = ForAll (Proxy @s)

exists :: forall t s. (KnownSMTRepr t, KnownSymbol s) => (Expr t -> Expr BoolType) -> Expr BoolType
exists = Exists (Proxy @s)