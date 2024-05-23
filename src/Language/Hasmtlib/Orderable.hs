module Language.Hasmtlib.Orderable where

import Language.Hasmtlib.Type.Expr
import Language.Hasmtlib.Equatable
  
class Equatable a => Orderable a where
  (<?)  :: a -> a -> Expr BoolType
  (<=?) :: a -> a -> Expr BoolType
  (>=?) :: a -> a -> Expr BoolType
  (>?)  :: a -> a -> Expr BoolType
infix 4 <?, <=?, >=?, >?

instance Orderable (Expr a) where
  (<?)  = LTH
  (<=?) = LTHE
  (>=?) = GTHE
  (>?)  = GTH