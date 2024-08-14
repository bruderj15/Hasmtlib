{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Hasmtlib.Internal.Expr where

import Prelude hiding ((&&))
import Language.Hasmtlib.Internal.Render
import Language.Hasmtlib.Type.ArrayMap
import Language.Hasmtlib.Type.SMTSort
import Language.Hasmtlib.Boolean
import Data.Map hiding (toList)
import Data.List (intercalate)
import Data.Proxy
import Data.Coerce
import Data.GADT.Compare
import Data.Foldable (toList)
import Data.ByteString.Builder
import qualified Data.Vector.Sized as V
import Control.Lens
import GHC.TypeLits

-- | An internal SMT variable with a phantom-type which holds an 'Int' as it's identifier.
type role SMTVar phantom
newtype SMTVar (t :: SMTSort) = SMTVar { _varId :: Int } deriving (Show, Eq, Ord)
$(makeLenses ''SMTVar)

-- | A wrapper for values of 'SMTSort's.
data Value (t :: SMTSort) where
  IntValue   :: HaskellType IntSort    -> Value IntSort
  RealValue  :: HaskellType RealSort   -> Value RealSort
  BoolValue  :: HaskellType BoolSort   -> Value BoolSort
  BvValue    :: KnownNat n => HaskellType (BvSort n) -> Value (BvSort n)
  ArrayValue :: (KnownSMTSort k, KnownSMTSort v, Ord (HaskellType k), Eq (HaskellType v))
    => HaskellType (ArraySort k v) -> Value (ArraySort k v)

-- | Unwrap a value from 'Value'.
unwrapValue :: Value t -> HaskellType t
unwrapValue (IntValue  v) = v
unwrapValue (RealValue v) = v
unwrapValue (BoolValue v) = v
unwrapValue (BvValue   v) = v
unwrapValue (ArrayValue v) = v
{-# INLINEABLE unwrapValue #-}

-- | Wrap a value into 'Value'.
wrapValue :: forall t. KnownSMTSort t => HaskellType t -> Value t
wrapValue = case sortSing @t of
  SIntSort  -> IntValue
  SRealSort -> RealValue
  SBoolSort -> BoolValue
  SBvSort _ -> BvValue
  SArraySort _ _ -> ArrayValue
{-# INLINEABLE wrapValue #-}

-- | An existential wrapper that hides some known 'SMTSort'.
type SomeKnownSMTSort f = SomeSMTSort '[KnownSMTSort] f

-- | A SMT expression.
--   For internal use only.
--   For building expressions use the corresponding instances (Num, Boolean, ...).
data Expr (t :: SMTSort) where
  Var       :: KnownSMTSort t => SMTVar t -> Expr t
  Constant  :: Value  t -> Expr t

  Plus      :: Num (HaskellType t) => Expr t -> Expr t -> Expr t
  Neg       :: Num (HaskellType t) => Expr t -> Expr t
  Mul       :: Num (HaskellType t) => Expr t -> Expr t -> Expr t
  Abs       :: Num (HaskellType t) => Expr t -> Expr t
  Mod       :: Expr IntSort  -> Expr IntSort  -> Expr IntSort
  IDiv      :: Expr IntSort  -> Expr IntSort  -> Expr IntSort
  Div       :: Expr RealSort -> Expr RealSort -> Expr RealSort

  LTH       :: (Ord (HaskellType t), KnownSMTSort t) => Expr t -> Expr t -> Expr BoolSort
  LTHE      :: (Ord (HaskellType t), KnownSMTSort t) => Expr t -> Expr t -> Expr BoolSort
  EQU       :: (Eq (HaskellType t), KnownSMTSort t, KnownNat n) => V.Vector (n + 2) (Expr t) -> Expr BoolSort
  Distinct  :: (Eq (HaskellType t), KnownSMTSort t, KnownNat n) => V.Vector (n + 2) (Expr t) -> Expr BoolSort
  GTHE      :: (Ord (HaskellType t), KnownSMTSort t) => Expr t -> Expr t -> Expr BoolSort
  GTH       :: (Ord (HaskellType t), KnownSMTSort t) => Expr t -> Expr t -> Expr BoolSort

  Not       :: Boolean (HaskellType t) => Expr t -> Expr t
  And       :: Boolean (HaskellType t) => Expr t -> Expr t -> Expr t
  Or        :: Boolean (HaskellType t) => Expr t -> Expr t -> Expr t
  Impl      :: Boolean (HaskellType t) => Expr t -> Expr t -> Expr t
  Xor       :: Boolean (HaskellType t) => Expr t -> Expr t -> Expr t

  Pi        :: Expr RealSort
  Sqrt      :: Expr RealSort -> Expr RealSort
  Exp       :: Expr RealSort -> Expr RealSort
  Sin       :: Expr RealSort -> Expr RealSort
  Cos       :: Expr RealSort -> Expr RealSort
  Tan       :: Expr RealSort -> Expr RealSort
  Asin      :: Expr RealSort -> Expr RealSort
  Acos      :: Expr RealSort -> Expr RealSort
  Atan      :: Expr RealSort -> Expr RealSort

  ToReal    :: Expr IntSort  -> Expr RealSort
  ToInt     :: Expr RealSort -> Expr IntSort
  IsInt     :: Expr RealSort -> Expr BoolSort

  Ite       :: Expr BoolSort -> Expr t -> Expr t -> Expr t

  BvNot     :: KnownNat n => Expr (BvSort n) -> Expr (BvSort n)
  BvAnd     :: KnownNat n => Expr (BvSort n) -> Expr (BvSort n) -> Expr (BvSort n)
  BvOr      :: KnownNat n => Expr (BvSort n) -> Expr (BvSort n) -> Expr (BvSort n)
  BvXor     :: KnownNat n => Expr (BvSort n) -> Expr (BvSort n) -> Expr (BvSort n)
  BvNand    :: KnownNat n => Expr (BvSort n) -> Expr (BvSort n) -> Expr (BvSort n)
  BvNor     :: KnownNat n => Expr (BvSort n) -> Expr (BvSort n) -> Expr (BvSort n)
  BvNeg     :: KnownNat n => Expr (BvSort n) -> Expr (BvSort n)
  BvAdd     :: KnownNat n => Expr (BvSort n) -> Expr (BvSort n) -> Expr (BvSort n)
  BvSub     :: KnownNat n => Expr (BvSort n) -> Expr (BvSort n) -> Expr (BvSort n)
  BvMul     :: KnownNat n => Expr (BvSort n) -> Expr (BvSort n) -> Expr (BvSort n)
  BvuDiv    :: KnownNat n => Expr (BvSort n) -> Expr (BvSort n) -> Expr (BvSort n)
  BvuRem    :: KnownNat n => Expr (BvSort n) -> Expr (BvSort n) -> Expr (BvSort n)
  BvShL     :: KnownNat n => Expr (BvSort n) -> Expr (BvSort n) -> Expr (BvSort n)
  BvLShR    :: KnownNat n => Expr (BvSort n) -> Expr (BvSort n) -> Expr (BvSort n)
  BvConcat  :: (KnownNat n, KnownNat m) => Expr (BvSort n) -> Expr (BvSort m) -> Expr (BvSort (n + m))
  BvRotL    :: (KnownNat n, KnownNat i, KnownNat (Mod i n)) => Proxy i -> Expr (BvSort n) -> Expr (BvSort n)
  BvRotR    :: (KnownNat n, KnownNat i, KnownNat (Mod i n)) => Proxy i -> Expr (BvSort n) -> Expr (BvSort n)
  BvuLT     :: KnownNat n => Expr (BvSort n) -> Expr (BvSort n) -> Expr BoolSort
  BvuLTHE   :: KnownNat n => Expr (BvSort n) -> Expr (BvSort n) -> Expr BoolSort
  BvuGTHE   :: KnownNat n => Expr (BvSort n) -> Expr (BvSort n) -> Expr BoolSort
  BvuGT     :: KnownNat n => Expr (BvSort n) -> Expr (BvSort n) -> Expr BoolSort

  ArrSelect :: (KnownSMTSort k, KnownSMTSort v, Ord (HaskellType k), Eq (HaskellType v)) => Expr (ArraySort k v) -> Expr k -> Expr v
  ArrStore  :: (KnownSMTSort k, KnownSMTSort v, Ord (HaskellType k)) => Expr (ArraySort k v) -> Expr k -> Expr v -> Expr (ArraySort k v)

  -- Just v if quantified var has been created already, Nothing otherwise
  ForAll    :: KnownSMTSort t => Maybe (SMTVar t) -> (Expr t -> Expr BoolSort) -> Expr BoolSort
  Exists    :: KnownSMTSort t => Maybe (SMTVar t) -> (Expr t -> Expr BoolSort) -> Expr BoolSort

instance GEq SMTVar where
  geq x y = _

instance GEq Value where
  geq (BoolValue x) (BoolValue y)   = if x == y then Just Refl else Nothing
  geq (IntValue x) (IntValue y)     = if x == y then Just Refl else Nothing
  geq (RealValue x) (RealValue y)   = if x == y then Just Refl else Nothing
  geq (BvValue x) (BvValue y)       = case cmpNat x y of
    EQI -> if x == y then Just Refl else Nothing
    _   -> Nothing
  geq ax@(ArrayValue x) ay@(ArrayValue y) = case geq (sortSing' ax) (sortSing' ay) of
    Nothing -> Nothing
    Just Refl -> if x == y then Just Refl else Nothing
  geq _ _ = Nothing

instance GEq Expr where
  geq (Var x) (Var y)           = geq x y
  geq (Constant x) (Constant y) = geq x y
  geq (Plus x y) (Plus x' y')   = case geq x x' of Nothing -> Nothing ; Just Refl -> geq y y'
  geq (Neg x) (Neg y)           = geq x y
  geq (Mul x y) (Mul x' y')     = case geq x x' of Nothing -> Nothing ; Just Refl -> geq y y'
  geq (Abs x) (Abs y)           = geq x y
  geq (Mod x y) (Mod x' y')     = case geq x x' of Nothing -> Nothing ; Just Refl -> geq y y'
  geq (IDiv x y) (IDiv x' y')   = case geq x x' of Nothing -> Nothing ; Just Refl -> geq y y'
  geq (LTH x y) (LTH x' y')     = case geq x x' of
    Nothing -> Nothing
    Just Refl -> case geq y y' of
      Nothing -> Nothing
      Just Refl -> Just Refl
  geq (LTHE x y) (LTHE x' y')     = case geq x x' of
    Nothing -> Nothing
    Just Refl -> case geq y y' of
      Nothing -> Nothing
      Just Refl -> Just Refl
  geq (EQU xs) (EQU ys)           = _
  geq (Distinct xs) (Distinct ys) = _
  geq (GTHE x y) (GTHE x' y')     = case geq x x' of
    Nothing -> Nothing
    Just Refl -> case geq y y' of
      Nothing -> Nothing
      Just Refl -> Just Refl
  geq (GTH x y) (GTH x' y')     = case geq x x' of
    Nothing -> Nothing
    Just Refl -> case geq y y' of
      Nothing -> Nothing
      Just Refl -> Just Refl
  geq _ _ = Nothing


instance Eq (Value t) => Eq (Expr t) where
  -- (Not x)            = vars1 x
  -- (And x y)          = vars1 x <> vars1 y
  -- (Or x y)           = vars1 x <> vars1 y
  -- (Impl x y)         = vars1 x <> vars1 y
  -- (Xor x y)          = vars1 x <> vars1 y
  -- Pi                 = mempty
  -- (Sqrt x)           = vars1 x
  -- (Exp x)            = vars1 x
  -- (Sin x)            = vars1 x
  -- (Cos x)            = vars1 x
  -- (Tan x)            = vars1 x
  -- (Asin x)           = vars1 x
  -- (Acos x)           = vars1 x
  -- (Atan x)           = vars1 x
  -- (ToReal x)         = vars1 x
  -- (ToInt x)          = vars1 x
  -- (IsInt x)          = vars1 x
  -- (Ite p t f)        = vars1 p <> vars1 t <> vars1 f
  -- (BvNot x)          = vars1 x
  -- (BvAnd x y)        = vars1 x <> vars1 y
  -- (BvOr x y)         = vars1 x <> vars1 y
  -- (BvXor x y)        = vars1 x <> vars1 y
  -- (BvNand x y)       = vars1 x <> vars1 y
  -- (BvNor x y)        = vars1 x <> vars1 y
  -- (BvNeg x)          = vars1 x
  -- (BvAdd x y)        = vars1 x <> vars1 y
  -- (BvSub x y)        = vars1 x <> vars1 y
  -- (BvMul x y)        = vars1 x <> vars1 y
  -- (BvuDiv x y)       = vars1 x <> vars1 y
  -- (BvuRem x y)       = vars1 x <> vars1 y
  -- (BvShL x y)        = vars1 x <> vars1 y
  -- (BvLShR x y)       = vars1 x <> vars1 y
  -- (BvConcat x y)     = vars1 x <> vars1 y
  -- (BvRotL _ x)       = vars1 x
  -- (BvRotR _ x)       = vars1 x
  -- (BvuLT x y)        = vars1 x <> vars1 y
  -- (BvuLTHE x y)      = vars1 x <> vars1 y
  -- (BvuGTHE x y)      = vars1 x <> vars1 y
  -- (BvuGT x y)        = vars1 x <> vars1 y
  -- (ArrSelect i arr)  = vars1 i <> vars1 arr
  -- (ArrStore i x arr) = vars1 i <> vars1 x <> vars1 arr
  -- (ForAll mQv expr)  = vars1Q mQv expr
  -- (Exists mQv expr)  = vars1Q mQv expr

instance Boolean (Expr BoolSort) where
  bool = Constant . BoolValue
  {-# INLINE bool #-}
  (&&) = And
  {-# INLINE (&&) #-}
  (||) = Or
  {-# INLINE (||) #-}
  not  = Not
  {-# INLINE not #-}
  xor  = Xor
  {-# INLINE xor #-}

instance KnownNat n => Boolean (Expr (BvSort n)) where
  bool = Constant . BvValue . bool
  {-# INLINE bool #-}
  (&&) = BvAnd
  {-# INLINE (&&) #-}
  (||) = BvOr
  {-# INLINE (||) #-}
  not  = BvNot
  {-# INLINE not #-}
  xor  = BvXor
  {-# INLINE xor #-}

instance Bounded (Expr BoolSort) where
  minBound = false
  maxBound = true

instance KnownNat n => Bounded (Expr (BvSort n)) where
  minBound = Constant $ BvValue minBound
  maxBound = Constant $ BvValue maxBound

instance Render (SMTVar t) where
  render v = "var_" <> intDec (coerce @(SMTVar t) @Int v)
  {-# INLINEABLE render #-}

instance Render (Value t) where
  render (IntValue x)   = render x
  render (RealValue x)  = render x
  render (BoolValue x)  = render x
  render (BvValue   v)  = "#b" <> render v
  render (ArrayValue arr) = case minViewWithKey (arr^.stored) of
    Nothing -> constRender $ arr^.arrConst
    Just ((k,v), stored')
      | size (arr^.stored) > 1 -> render $ ArrStore (Constant (wrapValue (arr & stored .~ stored'))) (Constant (wrapValue k)) (Constant (wrapValue v))
      | otherwise  -> constRender v
    where
      constRender v = "((as const " <> render (goSing arr) <> ") " <> render (wrapValue v) <> ")"
      goSing :: forall k v. (KnownSMTSort k, KnownSMTSort v, Ord (HaskellType k), Eq (HaskellType v))
        => ConstArray (HaskellType k) (HaskellType v) -> SSMTSort (ArraySort k v)
      goSing _ = sortSing @(ArraySort k v)

instance KnownSMTSort t => Render (Expr t) where
  render (Var v)      = render v
  render (Constant c) = render c

  render (Plus x y)   = renderBinary "+" x y
  render (Neg x)      = renderUnary  "-" x
  render (Mul x y)    = renderBinary "*" x y
  render (Abs x)      = renderUnary  "abs" x
  render (Mod x y)    = renderBinary "mod" x y
  render (IDiv x y)   = renderBinary "div" x y
  render (Div x y)    = renderBinary "/" x y

  render (LTH x y)    = renderBinary "<" x y
  render (LTHE x y)   = renderBinary "<=" x y
  render (EQU xs)     = renderNary "=" $ V.toList xs
  render (Distinct xs)= renderNary "distinct" $ V.toList xs
  render (GTHE x y)   = renderBinary ">=" x y
  render (GTH x y)    = renderBinary ">" x y

  render (Not x)      = renderUnary  "not" x
  render (And x y)    = renderBinary "and" x y
  render (Or x y)     = renderBinary "or" x y
  render (Impl x y)   = renderBinary "=>" x y
  render (Xor x y)    = renderBinary "xor" x y

  render Pi           = "real.pi"
  render (Sqrt x)     = renderUnary "sqrt" x
  render (Exp x)      = renderUnary "exp" x
  render (Sin x)      = renderUnary "sin" x
  render (Cos x)      = renderUnary "cos" x
  render (Tan x)      = renderUnary "tan" x
  render (Asin x)     = renderUnary "arcsin" x
  render (Acos x)     = renderUnary "arccos" x
  render (Atan x)     = renderUnary "arctan" x

  render (ToReal x)   = renderUnary "to_real" x
  render (ToInt x)    = renderUnary "to_int" x
  render (IsInt x)    = renderUnary "is_int" x

  render (Ite p t f)  = renderTernary "ite" p t f

  render (BvNot x)          = renderUnary  "bvnot"  (render x)
  render (BvAnd x y)        = renderBinary "bvand"  (render x) (render y)
  render (BvOr x y)         = renderBinary "bvor"   (render x) (render y)
  render (BvXor x y)        = renderBinary "bvxor"  (render x) (render y)
  render (BvNand x y)       = renderBinary "bvnand" (render x) (render y)
  render (BvNor x y)        = renderBinary "bvnor"  (render x) (render y)
  render (BvNeg x)          = renderUnary  "bvneg"  (render x)
  render (BvAdd x y)        = renderBinary "bvadd"  (render x) (render y)
  render (BvSub x y)        = renderBinary "bvsub"  (render x) (render y)
  render (BvMul x y)        = renderBinary "bvmul"  (render x) (render y)
  render (BvuDiv x y)       = renderBinary "bvudiv" (render x) (render y)
  render (BvuRem x y)       = renderBinary "bvurem" (render x) (render y)
  render (BvShL x y)        = renderBinary "bvshl"  (render x) (render y)
  render (BvLShR x y)       = renderBinary "bvlshr" (render x) (render y)
  render (BvConcat x y)     = renderBinary "concat" (render x) (render y)
  render (BvRotL i x)       = renderUnary (renderBinary "_" ("rotate_left"  :: Builder) (render (natVal i))) (render x)
  render (BvRotR i x)       = renderUnary (renderBinary "_" ("rotate_right" :: Builder) (render (natVal i))) (render x)
  render (BvuLT x y)        = renderBinary "bvult"  (render x) (render y)
  render (BvuLTHE x y)      = renderBinary "bvule"  (render x) (render y)
  render (BvuGTHE x y)      = renderBinary "bvuge"  (render x) (render y)
  render (BvuGT x y)        = renderBinary "bvugt"  (render x) (render y)

  render (ArrSelect a i)    = renderBinary  "select" (render a) (render i)
  render (ArrStore a i v)   = renderTernary "store"  (render a) (render i) (render v)

  render (ForAll mQvar f) = renderQuantifier "forall" mQvar f
  render (Exists mQvar f) = renderQuantifier "exists" mQvar f

renderQuantifier :: forall t. KnownSMTSort t => Builder -> Maybe (SMTVar t) -> (Expr t -> Expr BoolSort) -> Builder
renderQuantifier qname (Just qvar) f =
  renderBinary
    qname
    ("(" <> renderUnary (render qvar) (sortSing @t) <> ")")
    expr
  where
    expr = render $ f $ Var qvar
renderQuantifier _ Nothing _ = mempty

instance Show (Value t) where
  show (IntValue x)   = "IntValue "   ++ show x
  show (RealValue x)  = "RealValue "  ++ show x
  show (BoolValue x)  = "BoolValue "  ++ show x
  show (BvValue x)    = "BvValue "    ++ show x
  show (ArrayValue x) = "ArrValue: "  ++ show (render (ArrayValue x)) -- FIXME: This is bad but easy now

instance Show (Expr t) where
  show (Var v)              = show v
  show (Constant c)         = show c
  show (Plus x y)           = "(" ++ show x ++ " + " ++ show y ++ ")"
  show (Neg x)              = "(- " ++ show x ++ ")"
  show (Mul x y)            = "(" ++ show x ++ " * " ++ show y ++ ")"
  show (Abs x)              = "(abs " ++ show x ++ ")"
  show (Mod x y)            = "(" ++ show x ++ " mod " ++ show y ++ ")"
  show (IDiv x y)           = "(" ++ show x ++ " div " ++ show y ++ ")"
  show (Div x y)            = "(" ++ show x ++ " / " ++ show y ++ ")"
  show (LTH x y)            = "(" ++ show x ++ " < " ++ show y ++ ")"
  show (LTHE x y)           = "(" ++ show x ++ " <= " ++ show y ++ ")"
  show (EQU xs)             = "(= " ++ intercalate " " (show <$> toList xs) ++ ")"
  show (Distinct xs)        = "(distinct " ++ intercalate " " (show <$> toList xs) ++ ")"
  show (GTHE x y)           = "(" ++ show x ++ " >= " ++ show y ++ ")"
  show (GTH x y)            = "(" ++ show x ++ " > " ++ show y ++ ")"
  show (Not x)              = "(not " ++ show x ++ ")"
  show (And x y)            = "(" ++ show x ++ " && " ++ show y ++ ")"
  show (Or x y)             = "(" ++ show x ++ " || " ++ show y ++ ")"
  show (Impl x y)           = "(" ++ show x ++ " ==> " ++ show y ++ ")"
  show (Xor x y)            = "(" ++ show x ++ " xor " ++ show y ++ ")"
  show Pi                   = "pi"
  show (Sqrt x)             = "(sqrt "    ++ show x ++ ")"
  show (Exp x)              = "(exp "     ++ show x ++ ")"
  show (Sin x)              = "(sin "     ++ show x ++ ")"
  show (Cos x)              = "(cos "     ++ show x ++ ")"
  show (Tan x)              = "(tan "     ++ show x ++ ")"
  show (Asin x)             = "(arcsin "  ++ show x ++ ")"
  show (Acos x)             = "(arccos "  ++ show x ++ ")"
  show (Atan x)             = "(arctan "  ++ show x ++ ")"
  show (ToReal x)           = "(to_real " ++ show x ++ ")"
  show (ToInt x)            = "(to_int "  ++ show x ++ ")"
  show (IsInt x)            = "(is_int "  ++ show x ++ ")"
  show (Ite p t f)          = "(ite " ++ show p ++ " " ++ show t ++ " " ++ show f ++ ")"
  show (BvNot x)            = "(not "  ++ show x ++ ")"
  show (BvAnd x y)          = "(" ++ show x ++ " && " ++ show y ++ ")"
  show (BvOr x y)           = "(" ++ show x ++ " || " ++ show y ++ ")"
  show (BvXor x y)          = "(" ++ show x ++ " xor " ++ show y ++ ")"
  show (BvNand x y)         = "(" ++ show x ++ " nand " ++ show y ++ ")"
  show (BvNor x y)          = "(" ++ show x ++ " nor " ++ show y ++ ")"
  show (BvNeg x)            = "(- " ++ show x ++ ")"
  show (BvAdd x y)          = "(" ++ show x ++ " + " ++ show y ++ ")"
  show (BvSub x y)          = "(" ++ show x ++ " - " ++ show y ++ ")"
  show (BvMul x y)          = "(" ++ show x ++ " * " ++ show y ++ ")"
  show (BvuDiv x y)         = "(" ++ show x ++ " udiv " ++ show y ++ ")"
  show (BvuRem x y)         = "(" ++ show x ++ " urem " ++ show y ++ ")"
  show (BvShL x y)          = "(" ++ show x ++ " bvshl " ++ show y ++ ")"
  show (BvLShR x y)         = "(" ++ show x ++ " bvlshr " ++ show y ++ ")"
  show (BvConcat x y)       = "(" ++ show x ++ " bvconcat " ++ show y ++ ")"
  show (BvRotL i x)         = "(" ++ show x ++ " bvrotl " ++ show (natVal i) ++ ")"
  show (BvRotR i x)         = "(" ++ show x ++ " bvrotr " ++ show (natVal i) ++ ")"
  show (BvuLT x y)          = "(" ++ show x ++ " bvult " ++ show y ++ ")"
  show (BvuLTHE x y)        = "(" ++ show x ++ " bvule " ++ show y ++ ")"
  show (BvuGTHE x y)        = "(" ++ show x ++ " bvuge " ++ show y ++ ")"
  show (BvuGT x y)          = "(" ++ show x ++ " bvugt " ++ show y ++ ")"
  show (ForAll (Just qv) f) = "(forall " ++ show qv ++ ": " ++ show (f (Var qv)) ++ ")"
  show (ForAll Nothing f)   = "(forall var_-1: " ++ show (f (Var (SMTVar (-1)))) ++ ")"
  show (ArrSelect i arr)    = "(select " ++ show i ++ " " ++ show arr ++ ")"
  show (ArrStore i x arr)   = "(select " ++ show i ++ " " ++ show x ++ " " ++ show arr ++ ")"
  show (Exists (Just qv) f) = "(exists " ++ show qv ++ ": " ++ show (f (Var qv)) ++ ")"
  show (Exists Nothing f)   = "(exists var_-1: " ++ show (f (Var (SMTVar (-1)))) ++ ")"
