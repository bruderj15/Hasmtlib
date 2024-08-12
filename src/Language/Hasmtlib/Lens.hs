{-# LANGUAGE UndecidableInstances #-}

module Language.Hasmtlib.Lens where

import Language.Hasmtlib.Internal.Expr
import Language.Hasmtlib.Type.Expr
import Language.Hasmtlib.Type.SMTSort
import Language.Hasmtlib.Equatable
import Language.Hasmtlib.Orderable
import Language.Hasmtlib.Iteable
import Data.GADT.Compare
import Control.Lens

type instance Index   (Expr StringSort) = Expr IntSort
type instance IxValue (Expr StringSort) = Expr StringSort

instance Ixed (Expr StringSort) where
  ix i f s = f (strAt s i) <&> \a ->
    let l = strSubstring a 0 i
        r = strSubstring a i (strLength a)
     in l <> strReplace r (strAt a i) s

instance AsEmpty (Expr StringSort) where
  _Empty = prism'
    (const mempty)
    (\s -> ite (s === mempty) (Just ()) Nothing)

instance Prefixed (Expr StringSort) where
  prefixed p = prism'
    (p <>)
    (\s -> ite (p `strPrefixOf` s) (Just $ strReplace s p mempty) Nothing)

instance Suffixed (Expr StringSort) where
  suffixed qs = prism'
    (<> qs)
    (\s -> ite (qs `strSuffixOf` s) (Just $ strSubstring s 0 (strLength s - strLength qs)) Nothing)

instance Cons (Expr StringSort) (Expr StringSort) (Expr StringSort) (Expr StringSort) where
  _Cons = prism'
    (uncurry (<>))
    (\s -> ite (strLength s >? 0) (Just (strAt s 0, strSubstring s 1 (strLength s))) Nothing)

instance Snoc (Expr StringSort) (Expr StringSort) (Expr StringSort) (Expr StringSort) where
  _Snoc = prism'
    (uncurry (<>))
    (\s -> ite (strLength s >? 0) (Just (strSubstring s 0 (strLength s - 1), strAt s (strLength s - 1))) Nothing)

type instance Index   (Expr (ArraySort k v)) = Expr k
type instance IxValue (Expr (ArraySort k v)) = Expr v

instance (KnownSMTSort k, KnownSMTSort v, Ord (HaskellType k)) => Ixed (Expr (ArraySort k v)) where
  ix i f arr = f (select arr i) <&> store arr i

-- | **Caution for quantified expressions:** 'plate-function' @f@ will only be applied if quantification already has taken place.(&&)
--   Therefore make sure 'quantify' has been run before.
--   Otherwise the quantified expression and therefore all it's sub-expressions will not have @f@ applied.
instance KnownSMTSort t => Plated (Expr t) where
  plate _ expr@(Var _)            = pure expr
  plate _ expr@(Constant _)       = pure expr
  plate f (Plus x y)              = Plus <$> f x <*> f y
  plate f (Neg x)                 = Neg <$> f x
  plate f (Mul x y)               = Mul <$> f x <*> f y
  plate f (Abs x)                 = Abs <$> f x
  plate f (Mod x y)               = Mod <$> f x <*> f y
  plate f (IDiv x y)              = IDiv <$> f x <*> f y
  plate f (Div x y)               = Div <$> f x <*> f y
  plate f (LTH x y)               = LTH <$> somePlate f x <*> somePlate f y
  plate f (LTHE x y)              = LTHE <$> somePlate f x <*> somePlate f y
  plate f (EQU xs)                = EQU <$> traverse (somePlate f) xs
  plate f (Distinct xs)           = Distinct <$> traverse (somePlate f) xs
  plate f (GTHE x y)              = GTHE <$> somePlate f x <*> somePlate f y
  plate f (GTH x y)               = GTH <$> somePlate f x <*> somePlate f y
  plate f (Not x)                 = Not <$> somePlate f x
  plate f (And x y)               = And <$> somePlate f x <*> somePlate f y
  plate f (Or x y)                = Or <$> somePlate f x <*> somePlate f y
  plate f (Impl x y)              = Impl <$> somePlate f x <*> somePlate f y
  plate f (Xor x y)               = Xor <$> somePlate f x <*> somePlate f y
  plate _ Pi                      = pure Pi
  plate f (Sqrt x)                = Sqrt <$> f x
  plate f (Exp x)                 = Exp <$> f x
  plate f (Sin x)                 = Sin <$> f x
  plate f (Cos x)                 = Cos <$> f x
  plate f (Tan x)                 = Tan <$> f x
  plate f (Asin x)                = Asin <$> f x
  plate f (Acos x)                = Acos <$> f x
  plate f (Atan x)                = Atan <$> f x
  plate f (ToReal x)              = ToReal <$> somePlate f x
  plate f (ToInt x)               = ToInt <$> somePlate f x
  plate f (IsInt x)               = IsInt <$> somePlate f x
  plate f (Ite p t n)             = Ite <$> somePlate f p <*> f t <*> f n
  plate f (BvNot x)               = BvNot <$> f x
  plate f (BvAnd x y)             = BvAnd <$> f x <*> f y
  plate f (BvOr x y)              = BvOr <$> f x <*> f y
  plate f (BvXor x y)             = BvXor <$> f x <*> f y
  plate f (BvNand x y)            = BvNand <$> f x <*> f y
  plate f (BvNor x y)             = BvNor <$> f x <*> f y
  plate f (BvNeg x)               = BvNeg <$> f x
  plate f (BvAdd x y)             = BvAdd <$> f x <*> f y
  plate f (BvSub x y)             = BvSub <$> f x <*> f y
  plate f (BvMul x y)             = BvMul <$> f x <*> f y
  plate f (BvuDiv x y)            = BvuDiv <$> f x <*> f y
  plate f (BvuRem x y)            = BvuRem <$> f x <*> f y
  plate f (BvShL x y)             = BvShL <$> f x <*> f y
  plate f (BvLShR x y)            = BvLShR <$> f x <*> f y
  plate f (BvConcat x y)          = BvConcat <$> somePlate f x <*> somePlate f y
  plate f (BvRotL i x)            = BvRotL i <$> f x
  plate f (BvRotR i x)            = BvRotR i <$> f x
  plate f (BvuLT x y)             = BvuLT <$> somePlate f x <*> somePlate f y
  plate f (BvuLTHE x y)           = BvuLTHE <$> somePlate f x <*> somePlate f y
  plate f (BvuGTHE x y)           = BvuGTHE <$> somePlate f x <*> somePlate f y
  plate f (BvuGT x y)             = BvuGT <$> somePlate f x <*> somePlate f y
  plate f (ArrSelect i arr)       = ArrSelect i <$> somePlate f arr
  plate f (ArrStore i x arr)      = ArrStore i <$> somePlate f x <*> somePlate f arr
  plate f (StrConcat x y)         = StrConcat <$> f x <*> f y
  plate f (StrLength x)           = StrLength <$> somePlate f x
  plate f (StrLT x y)             = StrLT <$> somePlate f x <*> somePlate f y
  plate f (StrLTHE x y)           = StrLTHE <$> somePlate f x <*> somePlate f y
  plate f (StrAt x i)             = StrAt <$> f x <*> somePlate f i
  plate f (StrSubstring x i j)    = StrSubstring <$> f x <*> somePlate f i <*> somePlate f j
  plate f (StrPrefixOf x y)       = StrPrefixOf <$> somePlate f x <*> somePlate f y
  plate f (StrSuffixOf x y)       = StrSuffixOf <$> somePlate f x <*> somePlate f y
  plate f (StrContains x y)       = StrContains <$> somePlate f x <*> somePlate f y
  plate f (StrIndexOf x y i)      = StrIndexOf <$> somePlate f x <*> somePlate f y <*> f i
  plate f (StrReplace x y y')     = StrReplace <$> f x <*> f y <*> f y'
  plate f (StrReplaceAll x y y')  = StrReplaceAll <$> f x <*> f y <*> f y'
  plate f (ForAll (Just qv) expr) = ForAll (Just qv) . const <$> somePlate f (expr (Var qv))
  plate _ (ForAll Nothing expr)   = pure $ ForAll Nothing expr
  plate f (Exists (Just qv) expr) = Exists (Just qv) . const <$> somePlate f (expr (Var qv))
  plate _ (Exists Nothing expr)   = pure $ Exists Nothing expr

-- | Apply the 'plate'-function @f@ for given 'Expr' @expr@ if possible.
--   Otherwise try to apply @f@ for the children of @expr@.
--   **Caution for quantified expressions:** 'plate-function' @f@ will only be applied if quantification already has taken place.(&&)
--   Therefore make sure 'quantify' has been run before.
--   Otherwise the quantified expression and therefore all it's sub-expressions will not have @f@ applied.
somePlate :: forall t f. (KnownSMTSort t, Applicative f) => (Expr t -> f (Expr t)) -> (forall s. KnownSMTSort s => Expr s -> f (Expr s))
somePlate f expr = case geq (sortSing @t) (sortSing' expr) of
  Just Refl -> f expr
  Nothing   -> case expr of
    Var _                -> pure expr
    Constant _           -> pure expr
    Plus x y             -> Plus <$> somePlate f x <*> somePlate f y
    Neg x                -> Neg  <$> somePlate f x
    Mul x y              -> Mul  <$> somePlate f x <*> somePlate f y
    Abs x                -> Abs  <$> somePlate f x
    Mod x y              -> Mod  <$> somePlate f x <*> somePlate f y
    IDiv x y             -> IDiv <$> somePlate f x <*> somePlate f y
    Div x y              -> Div  <$> somePlate f x <*> somePlate f y
    LTH x y              -> LTH  <$> somePlate f x <*> somePlate f y
    LTHE x y             -> LTHE <$> somePlate f x <*> somePlate f y
    EQU xs               -> EQU  <$> traverse (somePlate f) xs
    Distinct xs          -> Distinct <$> traverse (somePlate f) xs
    GTHE x y             -> GTHE <$> somePlate f x <*> somePlate f y
    GTH x y              -> GTH  <$> somePlate f x <*> somePlate f y
    Not x                -> Not  <$> somePlate f x
    And x y              -> And  <$> somePlate f x <*> somePlate f y
    Or x y               -> Or   <$> somePlate f x <*> somePlate f y
    Impl x y             -> Impl <$> somePlate f x <*> somePlate f y
    Xor x y              -> Xor  <$> somePlate f x <*> somePlate f y
    Pi                   -> pure Pi
    Sqrt x               -> Sqrt <$> somePlate f x
    Exp x                -> Exp  <$> somePlate f x
    Sin x                -> Sin  <$> somePlate f x
    Cos x                -> Cos  <$> somePlate f x
    Tan x                -> Tan  <$> somePlate f x
    Asin x               -> Asin <$> somePlate f x
    Acos x               -> Acos <$> somePlate f x
    Atan x               -> Atan <$> somePlate f x
    ToReal x             -> ToReal <$> somePlate f x
    ToInt x              -> ToInt  <$> somePlate f x
    IsInt x              -> IsInt  <$> somePlate f x
    Ite p t n            -> Ite    <$> somePlate f p <*> somePlate f t <*> somePlate f n
    BvNot x              -> BvNot  <$> somePlate f x
    BvAnd x y            -> BvAnd  <$> somePlate f x <*> somePlate f y
    BvOr x y             -> BvOr   <$> somePlate f x <*> somePlate f y
    BvXor x y            -> BvXor  <$> somePlate f x <*> somePlate f y
    BvNand x y           -> BvNand <$> somePlate f x <*> somePlate f y
    BvNor x y            -> BvNor  <$> somePlate f x <*> somePlate f y
    BvNeg x              -> BvNeg  <$> somePlate f x
    BvAdd x y            -> BvAdd  <$> somePlate f x <*> somePlate f y
    BvSub x y            -> BvSub  <$> somePlate f x <*> somePlate f y
    BvMul x y            -> BvMul  <$> somePlate f x <*> somePlate f y
    BvuDiv x y           -> BvuDiv <$> somePlate f x <*> somePlate f y
    BvuRem x y           -> BvuRem <$> somePlate f x <*> somePlate f y
    BvShL x y            -> BvShL  <$> somePlate f x <*> somePlate f y
    BvLShR x y           -> BvLShR <$> somePlate f x <*> somePlate f y
    BvConcat x y         -> BvConcat <$> somePlate f x <*> somePlate f y
    BvRotL i x           -> BvRotL i <$> somePlate f x
    BvRotR i x           -> BvRotR i <$> somePlate f x
    BvuLT x y            -> BvuLT    <$> somePlate f x <*> somePlate f y
    BvuLTHE x y          -> BvuLTHE  <$> somePlate f x <*> somePlate f y
    BvuGTHE x y          -> BvuGTHE  <$> somePlate f x <*> somePlate f y
    BvuGT x y            -> BvuGT    <$> somePlate f x <*> somePlate f y
    ArrSelect i arr      -> ArrSelect i   <$> somePlate f arr
    ArrStore i x arr     -> ArrStore i    <$> somePlate f x <*> somePlate f arr
    StrConcat x y        -> StrConcat     <$> somePlate f x <*> somePlate f y
    StrLength x          -> StrLength     <$> somePlate f x
    StrLT x y            -> StrLT         <$> somePlate f x <*> somePlate f y
    StrLTHE x y          -> StrLTHE       <$> somePlate f x <*> somePlate f y
    StrAt x i            -> StrAt         <$> somePlate f x <*> somePlate f i
    StrSubstring x i j   -> StrSubstring  <$> somePlate f x <*> somePlate f i <*> somePlate f j
    StrPrefixOf x y      -> StrPrefixOf   <$> somePlate f x <*> somePlate f y
    StrSuffixOf x y      -> StrSuffixOf   <$> somePlate f x <*> somePlate f y
    StrContains x y      -> StrContains   <$> somePlate f x <*> somePlate f y
    StrIndexOf x y i     -> StrIndexOf    <$> somePlate f x <*> somePlate f y <*> somePlate f i
    StrReplace x y y'    -> StrReplace    <$> somePlate f x <*> somePlate f y <*> somePlate f y'
    StrReplaceAll x y y' -> StrReplaceAll <$> somePlate f x <*> somePlate f y <*> somePlate f y'
    ForAll (Just qv) qexpr -> ForAll (Just qv) . const <$> somePlate f (qexpr (Var qv))
    ForAll Nothing qexpr   -> pure $ ForAll Nothing qexpr
    Exists (Just qv) qexpr -> Exists (Just qv) . const <$> somePlate f (qexpr (Var qv))
    Exists Nothing qexpr   -> pure $ Exists Nothing qexpr
