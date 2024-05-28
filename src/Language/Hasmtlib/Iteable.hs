module Language.Hasmtlib.Iteable where

class Iteable a b where
  ite :: a -> b -> b -> b

instance Iteable Bool a where
  ite p t f = if p then t else f