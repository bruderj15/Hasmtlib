module Language.Hasmtlib.Iteable where

class Iteable a b where
  ite :: b -> a -> a -> a

instance Iteable a Bool where
  ite p t f = if p then t else f