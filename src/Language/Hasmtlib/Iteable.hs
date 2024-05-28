module Language.Hasmtlib.Iteable where

class Iteable b a where
  ite :: b -> a -> a -> a

instance Iteable Bool a where
  ite p t f = if p then t else f