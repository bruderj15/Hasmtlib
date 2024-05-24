module Language.Hasmtlib.Equatable where

import Data.Kind

class Equatable a where
  type EqResult a :: Type
  (===) :: a -> a -> EqResult a
  (/==) :: a -> a -> EqResult a
infix 4 ===, /==
