module Language.Hasmtlib.Boolean where

import Data.Bit
import Data.Coerce
import Data.Bits as Bits
import Data.Foldable (foldl')
import qualified Data.Vector.Unboxed.Sized as V
import GHC.TypeNats
  
class Boolean b where
  -- | Lift a 'Bool'
  bool :: Bool -> b

  -- |
  -- @'true' = 'bool' 'True'@
  true :: b
  true = bool True

  -- |
  -- @'false' = 'bool' 'False'@
  false :: b
  false = bool False

  -- | Logical conjunction.
  (&&&) :: b -> b -> b

  -- | Logical disjunction (inclusive or).
  (|||) :: b -> b -> b

  -- | Logical implication.
  (==>) :: b -> b -> b
  x ==> y = not' x ||| y

  -- | Logical negation
  not' :: b -> b

  -- | Exclusive-or
  xor :: b -> b -> b
 
  infixr 3 &&&
  infixr 2 |||
  infixr 0 ==>

-- | The logical conjunction of several values.
and' :: (Foldable t, Boolean b) => t b -> b
and' = foldl' (&&&) true

-- | The logical disjunction of several values.
or' :: (Foldable t, Boolean b) => t b -> b
or' = foldl' (|||) false

-- | The negated logical conjunction of several values.
--
-- @'nand' = 'neg' . 'and'@
nand :: (Foldable t, Boolean b) => t b -> b
nand = not' . and'

-- | The negated logical disjunction of several values.
--
-- @'nor' = 'neg' . 'or'@
nor :: (Foldable t, Boolean b) => t b -> b
nor = not' . or'

-- | The logical conjunction of the mapping of a function over several values.
all' :: (Foldable t, Boolean b) => (a -> b) -> t a -> b
all' p = foldl' (\acc b -> acc &&& p b) true

-- | The logical disjunction of the mapping of a function over several values.
any' :: (Foldable t, Boolean b) => (a -> b) -> t a -> b
any' p = foldl' (\acc b -> acc ||| p b) false

instance Boolean Bool where
  bool  = id
  true  = True
  false = False
  (&&&) = (&&)
  (|||) = (||)
  not'  = not
  xor   = (/=)
  
instance Boolean Bit where
  bool  = Bit
  (&&&)= (.&.) 
  (|||) = (.|.) 
  not'  = complement
  xor   = Bits.xor

-- | Bitwise operations
instance KnownNat n => Boolean (V.Vector n Bit) where
  bool   = V.replicate . coerce
  (&&&)  = V.zipWith (&&&)
  (|||)  = V.zipWith (|||)
  not'   = V.map not'
  xor    = V.zipWith Bits.xor