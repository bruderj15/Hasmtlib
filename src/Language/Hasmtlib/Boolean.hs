module Language.Hasmtlib.Boolean where

import Data.Bit
import Data.Coerce
import Data.Bits as Bits
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

  -- | The logical conjunction of several values.
  and' :: Foldable t => t b -> b
  and' = all' id

  -- | The logical disjunction of several values.
  or' :: Foldable t => t b -> b
  or' = any' id

  -- | The negated logical conjunction of several values.
  --
  -- @'nand' = 'neg' . 'and'@
  nand :: Foldable t => t b -> b
  nand = not' . and'

  -- | The negated logical disjunction of several values.
  --
  -- @'nor' = 'neg' . 'or'@
  nor :: Foldable t => t b -> b
  nor = not' . or'

  -- | The logical conjunction of the mapping of a function over several values.
  all' :: Foldable t => (a -> b) -> t a -> b

  -- | The logical disjunction of the mapping of a function over several values.
  any' :: Foldable t => (a -> b) -> t a -> b
  any' p  = not' . all' (not' . p)

  -- | Exclusive-or
  xor :: b -> b -> b
 
  infixr 3 &&&
  infixr 2 |||
  infixr 0 ==>

instance Boolean Bool where
  bool  = id
  true  = True
  false = False
  (&&&) = (&&)
  (|||) = (||)
  not'  = not
  and'  = and
  or'   = or
  all'  = all
  any'  = any
  xor   = (/=)
  
instance Boolean Bit where
  bool  = Bit
  (&&&)= (.&.) 
  (|||) = (.|.) 
  not'  = complement
  all' p = foldl (\b x -> p x &&& b) true
  xor   = Bits.xor
  
instance KnownNat n => Boolean (V.Vector n Bit) where
  bool   = V.replicate . coerce
  (&&&)  = V.zipWith (&&&)
  (|||)  = V.zipWith (|||)
  not'   = V.map not'
  and'   = foldl (&&&) true
  or'    = foldl (|||) false
  all' p = foldl (\b bv -> p bv &&& b) true
  xor    = V.zipWith Bits.xor