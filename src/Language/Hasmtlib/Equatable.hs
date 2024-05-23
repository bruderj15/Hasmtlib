module Language.Hasmtlib.Equatable where

import Prelude hiding (not)
import Language.Hasmtlib.Type.Expr
import Language.Hasmtlib.Boolean

class Equatable a where
  (===) :: a -> a -> Expr BoolType
  (/==) :: a -> a -> Expr BoolType
  x /== y = not' $ x === y
infix 4 ===, /==

instance KnownSMTRepr a => Equatable (Expr a) where
  (===) = EQU