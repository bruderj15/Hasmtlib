module Language.Hasmtlib.Iteable where

import Language.Hasmtlib.Internal.Expr
  
class Iteable b a where
  ite :: b -> a -> a -> a

instance Iteable (Expr BoolType) (Expr t) where
  ite = Ite

instance Iteable Bool a where
  ite p t f = if p then t else f