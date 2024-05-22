module Language.Hasmtlib.Orderable where

import Language.Hasmtlib.Type.SMT
import Language.Hasmtlib.Type.Expr
import Language.Hasmtlib.Equatable
  
class Equatable a => Orderable a where
  (<?)  :: a -> a -> Expr BoolType
  (<=?) :: a -> a -> Expr BoolType
  (>=?) :: a -> a -> Expr BoolType
  (>?)  :: a -> a -> Expr BoolType

instance Orderable (Expr a) where
  (<?)  = LTH
  (<=?) = LTHE
  (>=?) = GTHE
  (>?)  = GTH