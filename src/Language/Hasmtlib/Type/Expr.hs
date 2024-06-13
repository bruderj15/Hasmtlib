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

for_all :: KnownSMTRepr t => (Expr t -> Expr BoolType) -> Expr BoolType
for_all = ForAll

exists :: KnownSMTRepr t => (Expr t -> Expr BoolType) -> Expr BoolType
exists = Exists