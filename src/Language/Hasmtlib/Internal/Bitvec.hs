{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Language.Hasmtlib.Internal.Bitvec where

import Language.Hasmtlib.Boolean
import Data.Bit
import Data.Bits
import GHC.TypeNats
import qualified Data.Vector.Unboxed.Sized as V
import Data.Coerce

newtype Bitvec (n :: Nat) = Bitvec { unBitvec :: V.Vector n Bit }
  deriving stock (Eq, Ord)
  deriving newtype (Show, Boolean)


--instance KnownNat n => Num (Bitvec n) where
--  fromInteger