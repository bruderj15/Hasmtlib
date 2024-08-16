{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Hasmtlib.Internal.Expr where

import Language.Hasmtlib.Internal.Render
import Language.Hasmtlib.Type.ArrayMap
import Language.Hasmtlib.Type.SMTSort
import Language.Hasmtlib.Boolean
import Data.Map hiding (toList)
import Data.Proxy
import Data.Coerce
import Data.String (IsString(..))
import Data.Text (pack)
import Data.ByteString.Builder
import Data.ByteString.Lazy.UTF8 (toString)
import qualified Data.Vector.Sized as V
import Control.Lens
import GHC.TypeLits
import GHC.Generics

-- | An internal SMT variable with a phantom-type which holds an 'Int' as it's identifier.
type role SMTVar phantom
newtype SMTVar (t :: SMTSort) = SMTVar { _varId :: Int } deriving (Show, Eq, Ord, Generic)
$(makeLenses ''SMTVar)

-- | A wrapper for values of 'SMTSort's.
data Value (t :: SMTSort) where
  IntValue    :: HaskellType IntSort    -> Value IntSort
  RealValue   :: HaskellType RealSort   -> Value RealSort
  BoolValue   :: HaskellType BoolSort   -> Value BoolSort
  BvValue     :: HaskellType (BvSort n) -> Value (BvSort n)
  ArrayValue  :: (KnownSMTSort k, KnownSMTSort v, Ord (HaskellType k)) => HaskellType (ArraySort k v) -> Value (ArraySort k v)
  StringValue :: HaskellType StringSort -> Value StringSort

deriving instance Eq (HaskellType t) => Eq (Value t)
deriving instance Ord (HaskellType t) => Ord (Value t)

-- | Unwrap a value from 'Value'.
unwrapValue :: Value t -> HaskellType t
unwrapValue (IntValue  v)   = v
unwrapValue (RealValue v)   = v
unwrapValue (BoolValue v)   = v
unwrapValue (BvValue   v)   = v
unwrapValue (ArrayValue v)  = v
unwrapValue (StringValue v) = v
{-# INLINEABLE unwrapValue #-}

-- | Wrap a value into 'Value'.
wrapValue :: forall t. KnownSMTSort t => HaskellType t -> Value t
wrapValue = case sortSing @t of
  SIntSort       -> IntValue
  SRealSort      -> RealValue
  SBoolSort      -> BoolValue
  SBvSort _      -> BvValue
  SArraySort _ _ -> ArrayValue
  SStringSort    -> StringValue
{-# INLINEABLE wrapValue #-}

-- | An existential wrapper that hides some known 'SMTSort'.
type SomeKnownSMTSort f = SomeSMTSort '[KnownSMTSort] f

-- | Am SMT expression.
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

  ArrSelect :: (KnownSMTSort k, KnownSMTSort v, Ord (HaskellType k)) => Expr (ArraySort k v) -> Expr k -> Expr v
  ArrStore  :: (KnownSMTSort k, KnownSMTSort v, Ord (HaskellType k)) => Expr (ArraySort k v) -> Expr k -> Expr v -> Expr (ArraySort k v)

  StrConcat     :: Expr StringSort -> Expr StringSort -> Expr StringSort
  StrLength     :: Expr StringSort -> Expr IntSort
  StrLT         :: Expr StringSort -> Expr StringSort -> Expr BoolSort
  StrLTHE       :: Expr StringSort -> Expr StringSort -> Expr BoolSort
  StrAt         :: Expr StringSort -> Expr IntSort -> Expr StringSort
  StrSubstring  :: Expr StringSort -> Expr IntSort -> Expr IntSort -> Expr StringSort
  StrPrefixOf   :: Expr StringSort -> Expr StringSort -> Expr BoolSort
  StrSuffixOf   :: Expr StringSort -> Expr StringSort -> Expr BoolSort
  StrContains   :: Expr StringSort -> Expr StringSort -> Expr BoolSort
  StrIndexOf    :: Expr StringSort -> Expr StringSort -> Expr IntSort -> Expr IntSort
  StrReplace    :: Expr StringSort -> Expr StringSort -> Expr StringSort -> Expr StringSort
  StrReplaceAll :: Expr StringSort -> Expr StringSort -> Expr StringSort -> Expr StringSort

  -- Just v if quantified var has been created already, Nothing otherwise
  ForAll    :: KnownSMTSort t => Maybe (SMTVar t) -> (Expr t -> Expr BoolSort) -> Expr BoolSort
  Exists    :: KnownSMTSort t => Maybe (SMTVar t) -> (Expr t -> Expr BoolSort) -> Expr BoolSort

instance Boolean (Expr BoolSort) where
  bool = Constant . BoolValue
  {-# INLINE bool #-}
  (Constant (BoolValue x)) && y = if x then y else false
  x && (Constant (BoolValue y)) = if y then x else false
  x && y = And x y
  {-# INLINE (&&) #-}
  (Constant (BoolValue x)) || y = if x then true else y
  x || (Constant (BoolValue y)) = if y then true else x
  x || y = Or x y
  {-# INLINE (||) #-}
  not (Constant (BoolValue x)) = bool . Prelude.not $ x
  not x = Not x
  {-# INLINE not #-}
  xor (Constant (BoolValue x)) y  = if x then Language.Hasmtlib.Boolean.not y else y
  xor x (Constant (BoolValue y)) = if y then Language.Hasmtlib.Boolean.not x else x
  xor x y = Xor x y
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

instance Semigroup (Expr StringSort) where
  (<>) = StrConcat

instance Monoid (Expr StringSort) where
  mempty = Constant $ StringValue mempty
  mappend = (<>)

instance IsString (Expr StringSort) where
  fromString = Constant . StringValue . pack

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
      goSing :: forall k v. (KnownSMTSort k, KnownSMTSort v, Ord (HaskellType k)) => ConstArray (HaskellType k) (HaskellType v) -> SSMTSort (ArraySort k v)
      goSing _ = sortSing @(ArraySort k v)
  render (StringValue x) = "\"" <> render x <> "\""

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

  render (StrConcat x y)        = renderBinary "str.++"  (render x) (render y)
  render (StrLength x)          = renderUnary  "str.len" (render x)
  render (StrLT x y)            = renderBinary "str.<"   (render x) (render y)
  render (StrLTHE x y)          = renderBinary "str.<="  (render x) (render y)
  render (StrAt x i)            = renderBinary "str.at"  (render x) (render i)
  render (StrSubstring x i j)   = renderTernary "str.substr"  (render x) (render i) (render j)
  render (StrPrefixOf x y)      = renderBinary "str.prefixof" (render x) (render y)
  render (StrSuffixOf x y)      = renderBinary "str.suffixof" (render x) (render y)
  render (StrContains x y)      = renderBinary "str.contains" (render x) (render y)
  render (StrIndexOf x y i)     = renderTernary "str.indexof"     (render x) (render y) (render i)
  render (StrReplace x y y')    = renderTernary "str.replace"     (render x) (render y) (render y')
  render (StrReplaceAll x y y') = renderTernary "str.replace_all" (render x) (render y) (render y')

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
  show = toString . toLazyByteString . render

instance KnownSMTSort t => Show (Expr t) where
  show = toString . toLazyByteString . render
