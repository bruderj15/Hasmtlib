module Language.Hasmtlib.Boolean where

import Language.Hasmtlib.Type.Expr
import Data.Foldable (foldr')
  
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

instance Boolean (Expr BoolType) where
  bool    = Constant . BoolValue
  (&&&)   = And
  (|||)   = Or
  not'    = Not
  all' p  = foldr' (\expr acc -> acc &&& p expr) true
  any' p  = not' . all' (not' . p)
  xor     = Xor

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