{-# LANGUAGE NoImplicitPrelude #-}

{- |
This module provides a more generic version of bool-like algebras than what the Prelude does for 'Bool'.

The class 'Boolean' therefore overloads most of the Preludes operators like '(&&)'.

However, as 'Bool' also features an instance of 'Boolean', you can simply hide the Prelude ones - not having to import either qualified.

==== __Example__

@
import Prelude hiding (not, any, all, (&&), (||), and, or)
import Language.Hasmtlib

problem :: MonadSMT s m => StateT s m (Expr BoolSort, Expr BoolSort)
problem = do
  x <- var \@BoolSort
  y <- var
  let b = False || True
  assert $ x && y \<==\> and [x, y, true] && encode b ==> x && y
  return (x,y)
@
-}
module Language.Hasmtlib.Boolean
(
  -- * Class
  Boolean(..)

  -- * Operators
, and, or
, nand, nor
, all, any
)
where

import Prelude (Bool(..), (.), id, Eq(..))
import qualified Prelude as P
import Data.Bit
import Data.Coerce
import Data.Bits as Bits
import Data.Foldable hiding (and, or, all, any)
import qualified Data.Vector.Unboxed.Sized as V
import GHC.TypeNats

class Boolean b where
  -- | Lift a 'Bool'.
  bool :: Bool -> b

  -- | The true constant.
  -- @'true' = 'bool' 'True'@
  true :: b
  true = bool True

  -- | The false constant.
  -- @'false' = 'bool' 'False'@
  false :: b
  false = bool False

  -- | Logical conjunction.
  (&&) :: b -> b -> b

  -- | Logical disjunction (inclusive or).
  (||) :: b -> b -> b

  -- | Logical implication.
  (==>) :: b -> b -> b
  x ==> y = not x || y

  -- | Logical implication with arrow reversed.
  --
  -- @
  -- forall x y. (x ==> y) === (y <== x)
  -- @
  (<==) :: b -> b -> b
  y <== x = not x || y

  -- | Logical equivalence.
  (<==>) :: b -> b -> b
  x <==> y = (x ==> y) && (y ==> x)

  -- | Logical negation.
  not :: b -> b

  -- | Exclusive-or.
  xor :: b -> b -> b

  infix 4 `xor`
  infixr 4 <==>
  infixr 3 &&
  infixr 2 ||
  infixr 0 ==>
  infixl 0 <==

-- | The logical conjunction of several values.
and :: (Foldable t, Boolean b) => t b -> b
and = foldl (&&) true

-- | The logical disjunction of several values.
or :: (Foldable t, Boolean b) => t b -> b
or = foldl (||) false

-- | The negated logical conjunction of several values.
--
-- @'nand' = 'not' . 'and'@
nand :: (Foldable t, Boolean b) => t b -> b
nand = not . and

-- | The negated logical disjunction of several values.
--
-- @'nor' = 'not' . 'or'@
nor :: (Foldable t, Boolean b) => t b -> b
nor = not . or

-- | The logical conjunction of the mapping of a function over several values.
all :: (Foldable t, Boolean b) => (a -> b) -> t a -> b
all p = foldl (\acc b -> acc && p b) true

-- | The logical disjunction of the mapping of a function over several values.
any :: (Foldable t, Boolean b) => (a -> b) -> t a -> b
any p = foldl (\acc b -> acc || p b) false

instance Boolean Bool where
  bool   = id
  true   = True
  false  = False
  (&&)   = (P.&&)
  (||)   = (P.||)
  not    = P.not
  xor    = (/=)
  (<==>) = (==)

instance Boolean Bit where
  bool     = Bit
  (&&)     = (.&.)
  (||)     = (.|.)
  not      = complement
  xor      = Bits.xor
  x <==> y = bool (x == y)

-- | Defined bitwise
instance KnownNat n => Boolean (V.Vector n Bit) where
  bool = V.replicate . coerce
  (&&) = V.zipWith (&&)
  (||) = V.zipWith (||)
  not  = V.map not
  xor  = V.zipWith Bits.xor
  x <==> y = bool (x == y)
