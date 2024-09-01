{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Hasmtlib.Type.Relation
(
  -- * Relation type
  Relation(..)

  -- * Construction
, relation, symmetric_relation, build, buildFrom, buildFromM, identity

  -- * Accessors
, (!?), (!)
, bounds, indices, elems, assocs
, domain, codomain, image, preimage

  -- * Pretty printing
, table
)
where

import Prelude hiding (and, (&&), any)
import Language.Hasmtlib.Type.MonadSMT
import Language.Hasmtlib.Type.SMTSort
import Language.Hasmtlib.Type.Expr
import Language.Hasmtlib.Boolean
import Language.Hasmtlib.Codec
import Data.Coerce
import Data.Array (Array, Ix(..))
import Data.Maybe
import qualified Data.Array as A
import Control.Monad
import Control.Lens hiding (indices)

-- | @Relation a b@ represents a binary relation \(R \subseteq A \times B \),
-- where the domain \(A\) is a finite subset of the type @a@,
-- and the codomain \(B\) is a finite subset of the type @b@.
--
-- A relation is stored internally as @Array (a,b) Expr BoolSort@,
-- so @a@ and @b@ have to be instances of 'Ix',
-- and both \(A\) and \(B\) are intervals.
newtype Relation a b = Relation (Array (a, b) (Expr BoolSort))
  deriving stock Show

instance (Ix a, Ix b) => Codec (Relation a b) where
  type Decoded (Relation a b) = Array (a, b) Bool
  decode s (Relation x) = decode s x
  encode x = Relation $ encode x

instance (Ix a, Ix b, a ~ c, b ~ d) => Each (Relation a b) (Relation c d) (Expr BoolSort) (Expr BoolSort) where
  each f (Relation arr) = coerce <$> each f arr
  {-# INLINE each #-}

type instance Index (Relation a b) = (a,b)
type instance IxValue (Relation a b) = Expr BoolSort
instance (Ix a, Ix b) => Ixed (Relation a b) where
  ix i f (Relation arr) = coerce <$> ix i f arr
  {-# INLINE ix #-}

-- | @relation ((amin,bmin),(amax,mbax))@ constructs an indeterminate relation \(R \subseteq A \times B \)
-- where \(A\) is @{amin .. amax}@ and \(B\) is @{bmin .. bmax}@.
relation :: (Ix a, Ix b, MonadSMT s m) =>
  ((a,b),(a,b))
  -> m (Relation a b)
relation bnd = do
    pairs <- sequence $ do
        p <- A.range bnd
        return $ do
            x <- var
            return (p, x)
    return $ build bnd pairs

-- | Constructs an indeterminate relation \(R \subseteq B \times B \)
-- that is symmetric, i.e., \(\forall x, y \in B: ((x,y) \in R) \rightarrow ((y,x) \in R) \).
symmetric_relation ::
  (MonadSMT s m, Ix b) =>
  ((b, b), (b, b)) -- ^ Since a symmetric relation must be homogeneous, the domain must equal the codomain.
                   -- Therefore, given bounds @((p,q),(r,s))@, it must hold that @p=q@ and @r=s@.
  -> m (Relation b b)
symmetric_relation bnd = do
    pairs <- sequence $ do
        (p,q) <- A.range bnd
        guard $ p <= q
        return $ do
            x <- var
            return $   ((p,q), x)
                   : [ ((q,p), x) | p /= q ]
    return $ build bnd $ concat pairs

-- | Constructs a relation \(R \subseteq A \times B \) from a list.
build :: (Ix a, Ix b)
      => ((a,b),(a,b))
      -> [((a,b), Expr BoolSort)] -- ^ A list of tuples, where the first element represents an element
                           -- \((x,y) \in A \times B \) and the second element is a positive 'Expr' 'BoolSort'
                           -- if \((x,y) \in R \), or a negative 'Expr' 'BoolSort' if \((x,y) \notin R \).
      -> Relation a b
build bnd pairs = Relation $ A.array bnd pairs

-- | Constructs a relation \(R \subseteq A \times B \) from a function.
buildFrom :: (Ix a, Ix b)
          => ((a,b),(a,b))
          -> ((a,b) -> Expr BoolSort) -- ^ A function that assigns a 'Expr' 'BoolSort'-value to each element \((x,y) \in A \times B \).
          -> Relation a b
buildFrom bnd p = build bnd $ flip map (A.range bnd) $ \ i -> (i, p i)

-- | Constructs an indeterminate relation \(R \subseteq A \times B\) from a function.
buildFromM :: (Ix a, Ix b, MonadSMT s m)
          => ((a,b),(a,b))
          -> ((a,b) -> m (Expr BoolSort))
          -> m (Relation a b)
buildFromM bnd p = do
    pairs <- sequence $ do
        i <- A.range bnd
        return $ do
            x <- p i
            return (i, x)
    return $ build bnd pairs

-- | Constructs the identity relation \(I = \{ (x,x) ~|~ x \in A \} \subseteq A \times A\).
identity :: (Ix a)
         => ((a,a),(a,a)) -- ^ Since the identity relation is homogeneous, the domain must equal the codomain.
                          -- Therefore, given bounds @((p,q),(r,s))@, it must hold that @p=q@ and @r=s@.
         -> Relation a a
identity ((a,b),(c,d))
    | (a,c) == (b,d) = buildFrom ((a,b),(c,d)) (\ (i,j) -> bool $ i == j)
    | otherwise      = error "The domain must equal the codomain!"

-- | The bounds of the array that correspond to the matrix representation of the given relation.
bounds :: Relation a b -> ((a,b),(a,b))
bounds (Relation r) = A.bounds r
{-# INLINE bounds #-}

-- | The list of indices, where each index represents an element \((x,y) \in A \times B \)
-- that may be contained in the given relation \(R \subseteq A \times B \).
indices :: (Ix a, Ix b) => Relation a b -> [(a, b)]
indices (Relation r) = A.indices r
{-# INLINE indices #-}

-- | The list of tuples for the given relation \(R \subseteq A \times B \),
-- where the first element represents an element \((x,y) \in A \times B \)
-- and the second element indicates via a 'Expr' 'BoolSort' , if \((x,y) \in R \) or not.
assocs :: (Ix a, Ix b) => Relation a b -> [((a, b), Expr BoolSort)]
assocs (Relation r) = A.assocs r
{-# INLINE assocs #-}

-- | The list of elements of the array
-- that correspond to the matrix representation of the given relation.
elems :: Relation a b -> [Expr BoolSort]
elems (Relation r) = A.elems r
{-# INLINE elems #-}

-- | 'Maybe' ('Expr' 'BoolSort') for a given element \((x,y) \in A \times B \)
-- and a given relation \(R \subseteq A \times B \) that indicates
-- if \((x,y) \in R \) or not.
--
-- 'Just' if given element is in 'bounds' of the relation.
-- 'Nothing' otherwise.
(!?) :: (Ix a, Ix b) => Relation a b -> (a, b) -> Maybe (Expr BoolSort)
Relation r !? p = r^?ix p
{-# INLINE (!?) #-}

-- | Unsafe version of '(!?)'.
-- Produces an array-indexing-error if given element is not within the 'bounds' of the relation.
(!) :: (Ix a, Ix b) => Relation a b -> (a, b) -> Expr BoolSort
Relation r ! p = r A.! p
{-# INLINE (!) #-}

-- | The domain \(A\) of a relation \(R \subseteq A \times B\).
domain :: Ix a => Relation a b -> [a]
domain r =
  let ((x,_),(x',_)) = bounds r
  in A.range (x,x')
{-# INLINE domain #-}

-- | The codomain \(B\) of a relation \(R \subseteq A \times B\).
codomain :: Ix b => Relation a b -> [b]
codomain r =
  let ((_,y),(_,y')) = bounds r
  in A.range (y,y')
{-# INLINE codomain #-}

-- | Returns a list of 'Expr' 'BoolSort' indicating whether the projection on
-- given element \( x \in A \) holds for every element in the codomain:
--
-- \( \{ (x,y) \in R \mid y \in codomain(B) \} \)
image :: (Ix a, Ix b) => Relation a b -> a -> [Expr BoolSort]
image r x = mapMaybe ((r !?) . (x,)) (codomain r)
{-# INLINE image #-}

-- | Returns a list of 'Expr' 'BoolSort' indicating whether the projection on
-- given element \( y \in B \) holds for every element in the domain:
--
-- \( \{ (x,y) \in R \mid x \in domain(A) \} \)
preimage :: (Ix a, Ix b) => Relation a b -> b -> [Expr BoolSort]
preimage r y = mapMaybe ((r !?) . (,y)) (domain r)
{-# INLINE preimage #-}

-- | Print a satisfying assignment from an SMT solver, where the assignment is interpreted as a relation.
-- @putStrLn $ table \</assignment/\>@ corresponds to the matrix representation of this relation.
table :: (Ix a, Ix b)
      => Array (a,b) Bool -> String
table r = unlines $ do
    let ((a,b),(c,d)) = A.bounds r
    x <- A.range (a,c)
    return $ unwords $ do
        y <- A.range (b,d)
        return $ if r A.! (x,y) then "*" else "."
