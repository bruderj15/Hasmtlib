module Language.Hasmtlib.Orderable where

import Language.Hasmtlib.Equatable
import Data.Kind

class Equatable a => Orderable a where
  type OrdResult a :: Type
  (<=?) :: a -> a -> OrdResult a
  (>=?) :: a -> a -> OrdResult a
  (<?)  :: a -> a -> OrdResult a
  (>?)  :: a -> a -> OrdResult a
infix 4 <?, <=?, >=?, >?
