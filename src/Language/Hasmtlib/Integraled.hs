{-# LANGUAGE DefaultSignatures #-}

module Language.Hasmtlib.Integraled where

import qualified Prelude as P
import Numeric.Natural  
import Data.Word
import Data.Functor.Identity
import Data.Functor.Const
    
class Integraled a where
  quot :: a -> a -> a
  n `quot` d          =  q  where (q,_) = quotRem n d
  
  rem :: a -> a -> a
  n `rem` d           =  r  where (_,r) = quotRem n d

  div :: a -> a -> a
  n `div` d           =  q  where (q,_) = divMod n d

  mod :: a -> a -> a
  n `mod` d           =  r  where (_,r) = divMod n d  

  quotRem :: a -> a -> (a, a)
  default quotRem :: P.Integral a => a -> a -> (a, a)
  quotRem = P.quotRem
  
  divMod  :: a -> a -> (a, a)
  default divMod  :: P.Integral a => a -> a -> (a, a)
  divMod = P.quotRem
  
instance Integraled P.Int
instance Integraled P.Integer
instance Integraled P.Word
instance Integraled Natural
instance Integraled Word8
instance Integraled Word16
instance Integraled Word32
instance Integraled Word64
instance P.Integral a => Integraled (Identity a)
instance P.Integral a => Integraled (Const a b)