{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Hasmtlib.Internal.Expr where

import Language.Hasmtlib.Internal.Bitvec
import Language.Hasmtlib.Internal.Render
import Language.Hasmtlib.Boolean
import Data.GADT.Compare
import Data.Kind
import Data.Proxy
import Data.Coerce
import Data.ByteString.Builder
import Control.Lens
import GHC.TypeLits

-- | Sorts in SMTLib2 - used as promoted type (data-kind).
data SMTSort = BoolSort | IntSort | RealSort | BvSort Nat

-- | An internal SMT variable with a phantom-type which holds an 'Int' as it's identifier.
type role SMTVar phantom
newtype SMTVar (t :: SMTSort) = SMTVar { _varId :: Int } deriving (Show, Eq, Ord)
$(makeLenses ''SMTVar)

-- | Injective type-family that computes the Haskell 'Type' of a 'SMTSort'.
type family HaskellType (t :: SMTSort) = (r :: Type) | r -> t where
  HaskellType IntSort    = Integer
  HaskellType RealSort   = Double
  HaskellType BoolSort   = Bool
  HaskellType (BvSort n) = Bitvec n

-- | A wrapper for values of 'SMTSort's.
data Value (t :: SMTSort) where
  IntValue  :: HaskellType IntSort    -> Value IntSort
  RealValue :: HaskellType RealSort   -> Value RealSort
  BoolValue :: HaskellType BoolSort   -> Value BoolSort
  BvValue   :: HaskellType (BvSort n) -> Value (BvSort n)

-- | Unwrap a value.
unwrapValue :: Value t -> HaskellType t
unwrapValue (IntValue  v) = v
unwrapValue (RealValue v) = v
unwrapValue (BoolValue v) = v
unwrapValue (BvValue   v) = v
{-# INLINEABLE unwrapValue #-}

-- | Wrap a value.
wrapValue :: forall t. KnownSMTSort t => HaskellType t -> Value t
wrapValue = case sortSing @t of
  SIntSort  -> IntValue
  SRealSort -> RealValue
  SBoolSort -> BoolValue
  SBvSort _ -> BvValue
{-# INLINEABLE wrapValue #-}

deriving instance Show (Value t)
deriving instance Eq   (Value t)
deriving instance Ord  (Value t)

-- | Singleton for 'SMTSort'.
data SSMTSort (t :: SMTSort) where
  SIntSort  :: SSMTSort IntSort
  SRealSort :: SSMTSort RealSort
  SBoolSort :: SSMTSort BoolSort
  SBvSort   :: KnownNat n => Proxy n -> SSMTSort (BvSort n)

deriving instance Show (SSMTSort t)
deriving instance Eq   (SSMTSort t)
deriving instance Ord  (SSMTSort t)

instance GEq SSMTSort where
  geq SIntSort SIntSort       = Just Refl
  geq SRealSort SRealSort     = Just Refl
  geq SBoolSort SBoolSort     = Just Refl
  geq (SBvSort n) (SBvSort m) = case sameNat n m of
    Just Refl -> Just Refl
    Nothing   -> Nothing
  geq _ _                     = Nothing

instance GCompare SSMTSort where
  gcompare SBoolSort SBoolSort     = GEQ
  gcompare SIntSort SIntSort       = GEQ
  gcompare SRealSort SRealSort     = GEQ
  gcompare (SBvSort n) (SBvSort m) = case cmpNat n m of
    LTI -> GLT
    EQI -> GEQ
    GTI -> GGT
  gcompare SBoolSort _ = GLT
  gcompare _ SBoolSort = GGT
  gcompare SIntSort _  = GLT
  gcompare _ SIntSort  = GGT
  gcompare SRealSort _ = GLT
  gcompare _ SRealSort = GGT

-- | Compute singleton 'SSMTSort' from it's promoted type 'SMTSort'.
class    KnownSMTSort (t :: SMTSort)           where sortSing :: SSMTSort t
instance KnownSMTSort IntSort                  where sortSing = SIntSort
instance KnownSMTSort RealSort                 where sortSing = SRealSort
instance KnownSMTSort BoolSort                 where sortSing = SBoolSort
instance KnownNat n => KnownSMTSort (BvSort n) where sortSing = SBvSort (Proxy @n)

-- | Wrapper for 'sortSing' which takes a 'Proxy'
sortSing' :: forall prxy t. KnownSMTSort t => prxy t -> SSMTSort t
sortSing' _ = sortSing @t

-- | An existential wrapper that hides some 'SMTSort'.
data SomeKnownSMTSort f where
  SomeKnownSMTSort :: forall (t :: SMTSort) f. KnownSMTSort t => f t -> SomeKnownSMTSort f

-- | A SMT expression.
--   For internal use only.
--   For building expressions use the corresponding instances (Num, Boolean, ...).
data Expr (t :: SMTSort) where
  Var       :: SMTVar t -> Expr t
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
  EQU       :: (Eq  (HaskellType t), KnownSMTSort t) => Expr t -> Expr t -> Expr BoolSort
  Distinct  :: (Eq  (HaskellType t), KnownSMTSort t) => Expr t -> Expr t -> Expr BoolSort
  GTHE      :: (Ord (HaskellType t), KnownSMTSort t) => Expr t -> Expr t -> Expr BoolSort
  GTH       :: (Ord (HaskellType t), KnownSMTSort t) => Expr t -> Expr t -> Expr BoolSort

  Not       :: Boolean (HaskellType t) => Expr t -> Expr t
  And       :: Boolean (HaskellType t) => Expr t -> Expr t -> Expr t
  Or        :: Boolean (HaskellType t) => Expr t -> Expr t -> Expr t
  Impl      :: Boolean (HaskellType t) => Expr t -> Expr t -> Expr t
  Xor       :: Boolean (HaskellType t) => Expr t -> Expr t -> Expr t

  Pi       :: Expr RealSort
  Sqrt     :: Expr RealSort -> Expr RealSort
  Exp      :: Expr RealSort -> Expr RealSort
  Sin      :: Expr RealSort -> Expr RealSort
  Cos      :: Expr RealSort -> Expr RealSort
  Tan      :: Expr RealSort -> Expr RealSort
  Asin     :: Expr RealSort -> Expr RealSort
  Acos     :: Expr RealSort -> Expr RealSort
  Atan     :: Expr RealSort -> Expr RealSort

  ToReal   :: Expr IntSort  -> Expr RealSort
  ToInt    :: Expr RealSort -> Expr IntSort
  IsInt    :: Expr RealSort -> Expr BoolSort

  Ite      :: Expr BoolSort -> Expr t -> Expr t -> Expr t

  BvNot    :: KnownNat n => Expr (BvSort n) -> Expr (BvSort n)
  BvAnd    :: KnownNat n => Expr (BvSort n) -> Expr (BvSort n) -> Expr (BvSort n)
  BvOr     :: KnownNat n => Expr (BvSort n) -> Expr (BvSort n) -> Expr (BvSort n)
  BvXor    :: KnownNat n => Expr (BvSort n) -> Expr (BvSort n) -> Expr (BvSort n)
  BvNand   :: KnownNat n => Expr (BvSort n) -> Expr (BvSort n) -> Expr (BvSort n)
  BvNor    :: KnownNat n => Expr (BvSort n) -> Expr (BvSort n) -> Expr (BvSort n)
  BvNeg    :: KnownNat n => Expr (BvSort n) -> Expr (BvSort n)
  BvAdd    :: KnownNat n => Expr (BvSort n) -> Expr (BvSort n) -> Expr (BvSort n)
  BvSub    :: KnownNat n => Expr (BvSort n) -> Expr (BvSort n) -> Expr (BvSort n)
  BvMul    :: KnownNat n => Expr (BvSort n) -> Expr (BvSort n) -> Expr (BvSort n)
  BvuDiv   :: KnownNat n => Expr (BvSort n) -> Expr (BvSort n) -> Expr (BvSort n)
  BvuRem   :: KnownNat n => Expr (BvSort n) -> Expr (BvSort n) -> Expr (BvSort n)
  BvShL    :: KnownNat n => Expr (BvSort n) -> Expr (BvSort n) -> Expr (BvSort n)
  BvLShR   :: KnownNat n => Expr (BvSort n) -> Expr (BvSort n) -> Expr (BvSort n)
  BvConcat :: (KnownNat n, KnownNat m) => Expr (BvSort n) -> Expr (BvSort m) -> Expr (BvSort (n + m))
  BvRotL   :: (KnownNat n, KnownNat i, KnownNat (Mod i n)) => Proxy i -> Expr (BvSort n) -> Expr (BvSort n)
  BvRotR   :: (KnownNat n, KnownNat i, KnownNat (Mod i n)) => Proxy i -> Expr (BvSort n) -> Expr (BvSort n)
  BvuLT    :: KnownNat n => Expr (BvSort n) -> Expr (BvSort n) -> Expr BoolSort
  BvuLTHE  :: KnownNat n => Expr (BvSort n) -> Expr (BvSort n) -> Expr BoolSort
  BvuGTHE  :: KnownNat n => Expr (BvSort n) -> Expr (BvSort n) -> Expr BoolSort
  BvuGT    :: KnownNat n => Expr (BvSort n) -> Expr (BvSort n) -> Expr BoolSort

  ForAll   :: KnownSMTSort t => Maybe (SMTVar t) -> (Expr t -> Expr BoolSort) -> Expr BoolSort
  Exists   :: KnownSMTSort t => Maybe (SMTVar t) -> (Expr t -> Expr BoolSort) -> Expr BoolSort

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

instance Render (SSMTSort t) where
  render SBoolSort   = "Bool"
  render SIntSort    = "Int"
  render SRealSort   = "Real"
  render (SBvSort p) = renderBinary "_" ("BitVec" :: Builder) (natVal p)
  {-# INLINEABLE render #-}

instance Render (SMTVar t) where
  render v = "var_" <> intDec (coerce @(SMTVar t) @Int v)
  {-# INLINEABLE render #-}

instance KnownSMTSort t => Render (Expr t) where
  render (Var v)                  = render v
  render (Constant (BoolValue x)) = render x
  render (Constant (IntValue x))  = render x
  render (Constant (RealValue x)) = render x
  render (Constant (BvValue   v)) = "#b" <> render v

  render (Plus x y)   = renderBinary "+" x y
  render (Neg x)      = renderUnary  "-" x
  render (Mul x y)    = renderBinary "*" x y
  render (Abs x)      = renderUnary  "abs" x
  render (Mod x y)    = renderBinary "mod" x y
  render (IDiv x y)   = renderBinary "div" x y
  render (Div x y)    = renderBinary "/" x y

  render (LTH x y)    = renderBinary "<" x y
  render (LTHE x y)   = renderBinary "<=" x y
  render (EQU x y)    = renderBinary "=" x y
  render (Distinct x y) = renderBinary "distinct" x y
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

instance Show (Expr t) where
  show (Var v)                  = show v
  show (Constant (BoolValue x)) = show x
  show (Constant (IntValue x))  = show x
  show (Constant (RealValue x)) = show x
  show (Constant (BvValue   x)) = show x
  show (Plus x y)               = "(" ++ show x ++ " + " ++ show y ++ ")"
  show (Neg x)                  = "(- " ++ show x ++ ")"
  show (Mul x y)                = "(" ++ show x ++ " * " ++ show y ++ ")"
  show (Abs x)                  = "(abs " ++ show x ++ ")"
  show (Mod x y)                = "(" ++ show x ++ " mod " ++ show y ++ ")"
  show (IDiv x y)               = "(" ++ show x ++ " div " ++ show y ++ ")"
  show (Div x y)                = "(" ++ show x ++ " / " ++ show y ++ ")"
  show (LTH x y)                = "(" ++ show x ++ " < " ++ show y ++ ")"
  show (LTHE x y)               = "(" ++ show x ++ " <= " ++ show y ++ ")"
  show (EQU x y)                = "(" ++ show x ++ " == " ++ show y ++ ")"
  show (Distinct x y)           = "(" ++ show x ++ " /= " ++ show y ++ ")"
  show (GTHE x y)               = "(" ++ show x ++ " >= " ++ show y ++ ")"
  show (GTH x y)                = "(" ++ show x ++ " > " ++ show y ++ ")"
  show (Not x)                  = "(not " ++ show x ++ ")"
  show (And x y)                = "(" ++ show x ++ " && " ++ show y ++ ")"
  show (Or x y)                 = "(" ++ show x ++ " || " ++ show y ++ ")"
  show (Impl x y)               = "(" ++ show x ++ " ==> " ++ show y ++ ")"
  show (Xor x y)                = "(" ++ show x ++ " xor " ++ show y ++ ")"
  show Pi                       = "pi"
  show (Sqrt x)                 = "(sqrt "    ++ show x ++ ")"
  show (Exp x)                  = "(exp "     ++ show x ++ ")"
  show (Sin x)                  = "(sin "     ++ show x ++ ")"
  show (Cos x)                  = "(cos "     ++ show x ++ ")"
  show (Tan x)                  = "(tan "     ++ show x ++ ")"
  show (Asin x)                 = "(arcsin "  ++ show x ++ ")"
  show (Acos x)                 = "(arccos "  ++ show x ++ ")"
  show (Atan x)                 = "(arctan "  ++ show x ++ ")"
  show (ToReal x)               = "(to_real " ++ show x ++ ")"
  show (ToInt x)                = "(to_int "  ++ show x ++ ")"
  show (IsInt x)                = "(is_int "  ++ show x ++ ")"
  show (Ite p t f)              = "(ite " ++ show p ++ " " ++ show t ++ " " ++ show f ++ ")"
  show (BvNot x)                = "(not "  ++ show x ++ ")"
  show (BvAnd x y)              = "(" ++ show x ++ " && " ++ show y ++ ")"
  show (BvOr x y)               = "(" ++ show x ++ " || " ++ show y ++ ")"
  show (BvXor x y)              = "(" ++ show x ++ " xor " ++ show y ++ ")"
  show (BvNand x y)             = "(" ++ show x ++ " nand " ++ show y ++ ")"
  show (BvNor x y)              = "(" ++ show x ++ " nor " ++ show y ++ ")"
  show (BvNeg x)                = "(- " ++ show x ++ ")"
  show (BvAdd x y)              = "(" ++ show x ++ " + " ++ show y ++ ")"
  show (BvSub x y)              = "(" ++ show x ++ " - " ++ show y ++ ")"
  show (BvMul x y)              = "(" ++ show x ++ " * " ++ show y ++ ")"
  show (BvuDiv x y)             = "(" ++ show x ++ " udiv " ++ show y ++ ")"
  show (BvuRem x y)             = "(" ++ show x ++ " urem " ++ show y ++ ")"
  show (BvShL x y)              = "(" ++ show x ++ " bvshl " ++ show y ++ ")"
  show (BvLShR x y)             = "(" ++ show x ++ " bvlshr " ++ show y ++ ")"
  show (BvConcat x y)           = "(" ++ show x ++ " bvconcat " ++ show y ++ ")"
  show (BvRotL i x)             = "(" ++ show x ++ " bvrotl " ++ show (natVal i) ++ ")"
  show (BvRotR i x)             = "(" ++ show x ++ " bvrotr " ++ show (natVal i) ++ ")"
  show (BvuLT x y)              = "(" ++ show x ++ " bvult " ++ show y ++ ")"
  show (BvuLTHE x y)            = "(" ++ show x ++ " bvule " ++ show y ++ ")"
  show (BvuGTHE x y)            = "(" ++ show x ++ " bvuge " ++ show y ++ ")"
  show (BvuGT x y)              = "(" ++ show x ++ " bvugt " ++ show y ++ ")"
  show (ForAll (Just qv) f)     = "(forall " ++ show qv ++ ": " ++ show (f (Var qv)) ++ ")"
  show (ForAll Nothing f)       = "(forall var_-1: " ++ show (f (Var (SMTVar (-1)))) ++ ")"
  show (Exists (Just qv) f)     = "(exists " ++ show qv ++ ": " ++ show (f (Var qv)) ++ ")"
  show (Exists Nothing f)       = "(exists var_-1: " ++ show (f (Var (SMTVar (-1)))) ++ ")"