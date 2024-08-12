{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Hasmtlib.Type.Expr
 ( SMTVar(..), varId
 , Value(..), unwrapValue, wrapValue
 , Expr(..)
 , equal, distinct
 , bvShL, bvLShR, bvConcat, bvRotL, bvRotR
 , toIntSort, toRealSort, isIntSort
 , for_all , exists
 , select, store
 , strLength, strAt, strSubstring, strPrefixOf, strSuffixOf, strContains, strIndexOf, strReplace, strReplaceAll
 )
where

import Language.Hasmtlib.Internal.Expr
import Language.Hasmtlib.Internal.Expr.Num ()
import Language.Hasmtlib.Type.SMTSort
import Language.Hasmtlib.Boolean
import Data.Proxy
import Data.List (genericLength)
import Data.Foldable (toList)
import qualified Data.Vector.Sized as V
import GHC.TypeNats

-- | Test multiple expressions on equality within in the 'SMT'-Problem.
equal :: (Eq (HaskellType t), KnownSMTSort t, Foldable f) => f (Expr t) -> Expr BoolSort
equal (toList -> (a:b:xs)) = case someNatVal (genericLength xs) of
  SomeNat n -> case V.fromListN' n xs of
    Nothing  -> EQU $ V.fromTuple (a,b)
    Just xs' -> EQU $ xs' V.++ V.fromTuple (a,b)
equal (toList -> _)        = true

-- | Test multiple expressions on distinctness within in the 'SMT'-Problem.
distinct :: (Eq (HaskellType t), KnownSMTSort t, Foldable f) => f (Expr t) -> Expr BoolSort
distinct (toList -> (a:b:xs)) = case someNatVal (genericLength xs) of
  SomeNat n -> case V.fromListN' n xs of
    Nothing  -> Distinct $ V.fromTuple (a,b)
    Just xs' -> Distinct $ xs' V.++ V.fromTuple (a,b)
distinct (toList -> _)        = true

-- | A universal quantification for any specific 'SMTSort'.
--   If the type cannot be inferred, apply a type-annotation.
--   Nested quantifiers are also supported.
--
--   Usage:
--
--   @
--   assert $
--      for_all @IntSort $ \x ->
--         x + 0 === x && 0 + x === x
--   @
--
--   The lambdas 'x' is all-quantified here.
--   It will only be scoped for the lambdas body.
for_all :: forall t. KnownSMTSort t => (Expr t -> Expr BoolSort) -> Expr BoolSort
for_all = ForAll Nothing

-- | An existential quantification for any specific 'SMTSort'
--   If the type cannot be inferred, apply a type-annotation.
--   Nested quantifiers are also supported.
--
--   Usage:
--
--   @
--   assert $
--      for_all @(BvSort 8) $ \x ->
--          exists $ \y ->
--            x - y === 0
--   @
--
--   The lambdas 'y' is existentially quantified here.
--   It will only be scoped for the lambdas body.
exists :: forall t. KnownSMTSort t => (Expr t -> Expr BoolSort) -> Expr BoolSort
exists = Exists Nothing

-- | Select a value from an array.
select :: (KnownSMTSort k, KnownSMTSort v, Ord (HaskellType k)) => Expr (ArraySort k v) -> Expr k -> Expr v
select = ArrSelect

-- | Store a value in an array.
store :: (KnownSMTSort k, KnownSMTSort v, Ord (HaskellType k)) => Expr (ArraySort k v) -> Expr k -> Expr v -> Expr (ArraySort k v)
store = ArrStore

-- | Bitvector shift left
bvShL    :: KnownNat n => Expr (BvSort n) -> Expr (BvSort n) -> Expr (BvSort n)
bvShL    = BvShL
{-# INLINE bvShL #-}

-- | Bitvector logical shift right
bvLShR   :: KnownNat n => Expr (BvSort n) -> Expr (BvSort n) -> Expr (BvSort n)
bvLShR   = BvLShR
{-# INLINE bvLShR #-}

-- | Concat two bitvectors
bvConcat :: (KnownNat n, KnownNat m) => Expr (BvSort n) -> Expr (BvSort m) -> Expr (BvSort (n + m))
bvConcat = BvConcat
{-# INLINE bvConcat #-}

-- | Rotate bitvector left
bvRotL   :: (KnownNat n, KnownNat i, KnownNat (Mod i n)) => Proxy i -> Expr (BvSort n) -> Expr (BvSort n)
bvRotL   = BvRotL
{-# INLINE bvRotL #-}

-- | Rotate bitvector right
bvRotR   :: (KnownNat n, KnownNat i, KnownNat (Mod i n)) => Proxy i -> Expr (BvSort n) -> Expr (BvSort n)
bvRotR   = BvRotR
{-# INLINE bvRotR #-}

-- | Converts an expression of type 'IntSort' to type 'RealSort'.
toRealSort :: Expr IntSort  -> Expr RealSort
toRealSort = ToReal

-- | Converts an expression of type 'RealSort' to type 'IntSort'.
toIntSort :: Expr RealSort -> Expr IntSort
toIntSort = ToInt

-- | Checks whether an expression of type 'RealSort' may be safely converted to type 'IntSort'.
isIntSort :: Expr RealSort -> Expr BoolSort
isIntSort = IsInt

-- | Length of a string.
strLength :: Expr StringSort -> Expr IntSort
strLength = StrLength

-- | Singleton string containing a character at given position
--   or empty string when position is out of range.
--   The leftmost position is 0.
strAt :: Expr StringSort -> Expr IntSort -> Expr StringSort
strAt = StrAt

-- | @(strSubstring s i n)@ evaluates to the longest (unscattered) substring
--   of @s@ of length at most @n@ starting at position @i@.
--   It evaluates to the empty string if @n@ is negative or @i@ is not in
--   the interval @[0,l-1]@ where @l@ is the length of @s@.
strSubstring :: Expr StringSort -> Expr IntSort -> Expr IntSort -> Expr StringSort
strSubstring = StrSubstring

-- | First string is a prefix of second one.
--   @(str.prefixof s t)@ is @true@ iff @s@ is a prefix of @t@.
strPrefixOf :: Expr StringSort -> Expr StringSort -> Expr BoolSort
strPrefixOf = StrPrefixOf

-- | First string is a suffix of second one.
--   @(str.suffixof s t)@ is @true@ iff @s@ is a suffix of @t@.
strSuffixOf :: Expr StringSort -> Expr StringSort -> Expr BoolSort
strSuffixOf = StrSuffixOf

-- | First string contains second one
--   @(str.contains s t)@ iff @s@ contains @t@.
strContains :: Expr StringSort -> Expr StringSort -> Expr BoolSort
strContains = StrContains

-- | Index of first occurrence of second string in first one starting at the position specified by the third argument.
--   @(str.indexof s t i)@, with @0 <= i <= |s|@ is the position of the first
--   occurrence of @t@ in @s@ at or after position @i@, if any.
--   Otherwise, it is @-1@. Note that the result is @i@ whenever @i@ is within
--   the range @[0, |s|]@ and @t@ is empty.
strIndexOf :: Expr StringSort -> Expr StringSort -> Expr IntSort -> Expr IntSort
strIndexOf = StrIndexOf

-- | @(str.replace s t t')@ is the string obtained by replacing the first
--   occurrence of @t@ in @s@, if any, by @t'@. Note that if @t@ is empty, the
--   result is to prepend @t'@ to @s@; also, if @t@ does not occur in @s@ then
--   the result is @s@.
strReplace :: Expr StringSort -> Expr StringSort -> Expr StringSort -> Expr StringSort
strReplace = StrReplace

-- | @(str.replace_all s t t’)@ is @s@ if @t@ is the empty string. Otherwise, it
--   is the string obtained from @s@ by replacing all occurrences of @t@ in @s@
--   by @t’@, starting with the first occurrence and proceeding in left-to-right order.
strReplaceAll :: Expr StringSort -> Expr StringSort -> Expr StringSort -> Expr StringSort
strReplaceAll = StrReplaceAll
