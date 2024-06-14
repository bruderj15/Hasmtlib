{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Hasmtlib.Internal.Expr where

import Language.Hasmtlib.Internal.Bitvec
import Language.Hasmtlib.Internal.Render
import Language.Hasmtlib.Boolean
import Data.Kind
import Data.Proxy
import Data.Coerce
import Data.ByteString.Builder
import Control.Lens
import GHC.TypeLits

-- | Types of variables in SMTLib - used as promoted Type
data SMTType = IntType | RealType | BoolType | BvType Nat

-- | SMT variable
type role SMTVar phantom
newtype SMTVar (t :: SMTType) = SMTVar { _varId :: Int } deriving (Show, Eq, Ord)
$(makeLenses ''SMTVar)

-- | Computes the Haskell type of the SMTLib-Type
type family ValueType (t :: SMTType) = (r :: Type) | r -> t where
  ValueType IntType    = Integer
  ValueType RealType   = Double
  ValueType BoolType   = Bool
  ValueType (BvType n) = Bitvec n

-- | SMT value
data Value (t :: SMTType) where
  IntValue  :: ValueType IntType    -> Value IntType
  RealValue :: ValueType RealType   -> Value RealType
  BoolValue :: ValueType BoolType   -> Value BoolType
  BvValue   :: ValueType (BvType n) -> Value (BvType n)

extractValue :: Value t -> ValueType t
extractValue (IntValue  v) = v
extractValue (RealValue v) = v
extractValue (BoolValue v) = v
extractValue (BvValue   v) = v
{-# INLINEABLE extractValue #-}

putValue :: forall t. KnownSMTRepr t => ValueType t -> Value t
putValue = case singRepr @t of
  IntRepr  -> IntValue 
  RealRepr -> RealValue
  BoolRepr -> BoolValue
  BvRepr _ -> BvValue
{-# INLINEABLE putValue #-}

deriving instance Show (Value t)
deriving instance Eq   (Value t)
deriving instance Ord  (Value t)

-- | Singleton-Representation of the SMTLib-Type
data Repr (t :: SMTType) where
  IntRepr  :: Repr IntType
  RealRepr :: Repr RealType
  BoolRepr :: Repr BoolType
  BvRepr :: KnownNat n => Proxy n -> Repr (BvType n)

deriving instance Show (Repr t)
deriving instance Eq   (Repr t)
deriving instance Ord  (Repr t)

-- | Compute singletons @Repr t@ for @t :: SMTType@
class    KnownSMTRepr (t :: SMTType)           where singRepr :: Repr t
instance KnownSMTRepr IntType                  where singRepr = IntRepr
instance KnownSMTRepr RealType                 where singRepr = RealRepr
instance KnownSMTRepr BoolType                 where singRepr = BoolRepr
instance KnownNat n => KnownSMTRepr (BvType n) where singRepr = BvRepr (Proxy @n)

-- | Existential for KnownSMTRepr t
data SomeKnownSMTRepr f where
  SomeKnownSMTRepr :: forall (t :: SMTType) f. KnownSMTRepr t => f t -> SomeKnownSMTRepr f

-- | SMT Expression
data Expr (t :: SMTType) where
  -- Terms
  Var       :: SMTVar t -> Expr t
  Constant  :: Value  t -> Expr t
  Plus      :: Num (ValueType t) => Expr t -> Expr t -> Expr t
  Neg       :: Num (ValueType t) => Expr t -> Expr t
  Mul       :: Num (ValueType t) => Expr t -> Expr t -> Expr t
  Abs       :: Num (ValueType t) => Expr t -> Expr t
  Mod       :: Expr IntType  -> Expr IntType  -> Expr IntType
  IDiv      :: Expr IntType  -> Expr IntType  -> Expr IntType
  Div       :: Expr RealType -> Expr RealType -> Expr RealType
  
  -- Atoms
  LTH       :: (Ord (ValueType t), KnownSMTRepr t) => Expr t -> Expr t -> Expr BoolType
  LTHE      :: (Ord (ValueType t), KnownSMTRepr t) => Expr t -> Expr t -> Expr BoolType
  EQU       :: (Eq  (ValueType t), KnownSMTRepr t) => Expr t -> Expr t -> Expr BoolType
  Distinct  :: (Eq  (ValueType t), KnownSMTRepr t) => Expr t -> Expr t -> Expr BoolType
  GTHE      :: (Ord (ValueType t), KnownSMTRepr t) => Expr t -> Expr t -> Expr BoolType
  GTH       :: (Ord (ValueType t), KnownSMTRepr t) => Expr t -> Expr t -> Expr BoolType

  -- Formulas
  Not       :: Boolean (ValueType t) => Expr t -> Expr t
  And       :: Boolean (ValueType t) => Expr t -> Expr t -> Expr t
  Or        :: Boolean (ValueType t) => Expr t -> Expr t -> Expr t
  Impl      :: Boolean (ValueType t) => Expr t -> Expr t -> Expr t
  Xor       :: Boolean (ValueType t) => Expr t -> Expr t -> Expr t

  -- Transcendentals
  Pi       :: Expr RealType
  Sqrt     :: Expr RealType -> Expr RealType
  Exp      :: Expr RealType -> Expr RealType
  Sin      :: Expr RealType -> Expr RealType
  Cos      :: Expr RealType -> Expr RealType
  Tan      :: Expr RealType -> Expr RealType
  Asin     :: Expr RealType -> Expr RealType
  Acos     :: Expr RealType -> Expr RealType
  Atan     :: Expr RealType -> Expr RealType

  -- Conversion
  ToReal   :: Expr IntType  -> Expr RealType
  ToInt    :: Expr RealType -> Expr IntType
  IsInt    :: Expr RealType -> Expr BoolType

  -- Choosing
  Ite      :: Expr BoolType -> Expr t -> Expr t -> Expr t

  -- Bitvectors
  BvNot    :: KnownNat n => Expr (BvType n) -> Expr (BvType n)
  BvAnd    :: KnownNat n => Expr (BvType n) -> Expr (BvType n) -> Expr (BvType n)
  BvOr     :: KnownNat n => Expr (BvType n) -> Expr (BvType n) -> Expr (BvType n)
  BvXor    :: KnownNat n => Expr (BvType n) -> Expr (BvType n) -> Expr (BvType n)
  BvNand   :: KnownNat n => Expr (BvType n) -> Expr (BvType n) -> Expr (BvType n)
  BvNor    :: KnownNat n => Expr (BvType n) -> Expr (BvType n) -> Expr (BvType n)
  BvNeg    :: KnownNat n => Expr (BvType n) -> Expr (BvType n)
  BvAdd    :: KnownNat n => Expr (BvType n) -> Expr (BvType n) -> Expr (BvType n)
  BvSub    :: KnownNat n => Expr (BvType n) -> Expr (BvType n) -> Expr (BvType n)
  BvMul    :: KnownNat n => Expr (BvType n) -> Expr (BvType n) -> Expr (BvType n)
  BvuDiv   :: KnownNat n => Expr (BvType n) -> Expr (BvType n) -> Expr (BvType n)
  BvuRem   :: KnownNat n => Expr (BvType n) -> Expr (BvType n) -> Expr (BvType n)
  
  BvShL    :: KnownNat n => Expr (BvType n) -> Expr (BvType n) -> Expr (BvType n)
  BvLShR   :: KnownNat n => Expr (BvType n) -> Expr (BvType n) -> Expr (BvType n)
  BvConcat :: (KnownNat n, KnownNat m) => Expr (BvType n) -> Expr (BvType m) -> Expr (BvType (n + m))
  BvRotL   :: (KnownNat n, KnownNat i, KnownNat (Mod i n)) => Proxy i -> Expr (BvType n) -> Expr (BvType n)
  BvRotR   :: (KnownNat n, KnownNat i, KnownNat (Mod i n)) => Proxy i -> Expr (BvType n) -> Expr (BvType n)

  BvuLT    :: KnownNat n => Expr (BvType n) -> Expr (BvType n) -> Expr BoolType
  BvuLTHE  :: KnownNat n => Expr (BvType n) -> Expr (BvType n) -> Expr BoolType
  BvuGTHE  :: KnownNat n => Expr (BvType n) -> Expr (BvType n) -> Expr BoolType
  BvuGT    :: KnownNat n => Expr (BvType n) -> Expr (BvType n) -> Expr BoolType

  ForAll   :: KnownSMTRepr t => Maybe (SMTVar t) -> (Expr t -> Expr BoolType) -> Expr BoolType
  Exists   :: KnownSMTRepr t => Maybe (SMTVar t) -> (Expr t -> Expr BoolType) -> Expr BoolType

instance Boolean (Expr BoolType) where
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
  
instance KnownNat n => Boolean (Expr (BvType n)) where
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
  
instance Bounded (Expr BoolType) where
  minBound = false
  maxBound = true
  
instance KnownNat n => Bounded (Expr (BvType n)) where
  minBound = Constant $ BvValue minBound
  maxBound = Constant $ BvValue maxBound

instance RenderSMTLib2 (Repr t) where
  renderSMTLib2 IntRepr    = "Int"
  renderSMTLib2 RealRepr   = "Real"
  renderSMTLib2 BoolRepr   = "Bool"
  renderSMTLib2 (BvRepr p) = renderBinary "_" ("BitVec" :: Builder) (natVal p)
  {-# INLINEABLE renderSMTLib2 #-}

instance RenderSMTLib2 (SMTVar t) where
  renderSMTLib2 v = "var_" <> intDec (coerce @(SMTVar t) @Int v)
  {-# INLINEABLE renderSMTLib2 #-}

instance KnownSMTRepr t => RenderSMTLib2 (Expr t) where
  renderSMTLib2 (Var v)                  = renderSMTLib2 v
  renderSMTLib2 (Constant (IntValue x))  = renderSMTLib2 x
  renderSMTLib2 (Constant (RealValue x)) = renderSMTLib2 x
  renderSMTLib2 (Constant (BoolValue x)) = renderSMTLib2 x
  renderSMTLib2 (Constant (BvValue   v)) = "#b" <> renderSMTLib2 v

  renderSMTLib2 (Plus x y)   = renderBinary "+" x y
  renderSMTLib2 (Neg x)      = renderUnary  "-" x
  renderSMTLib2 (Mul x y)    = renderBinary "*" x y
  renderSMTLib2 (Abs x)      = renderUnary  "abs" x
  renderSMTLib2 (Mod x y)    = renderBinary "mod" x y
  renderSMTLib2 (IDiv x y)   = renderBinary "div" x y
  renderSMTLib2 (Div x y)    = renderBinary "/" x y

  renderSMTLib2 (LTH x y)    = renderBinary "<" x y
  renderSMTLib2 (LTHE x y)   = renderBinary "<=" x y
  renderSMTLib2 (EQU x y)    = renderBinary "=" x y
  renderSMTLib2 (Distinct x y) = renderBinary "distinct" x y
  renderSMTLib2 (GTHE x y)   = renderBinary ">=" x y
  renderSMTLib2 (GTH x y)    = renderBinary ">" x y

  renderSMTLib2 (Not x)      = renderUnary  "not" x
  renderSMTLib2 (And x y)    = renderBinary "and" x y
  renderSMTLib2 (Or x y)     = renderBinary "or" x y
  renderSMTLib2 (Impl x y)   = renderBinary "=>" x y
  renderSMTLib2 (Xor x y)    = renderBinary "xor" x y

  renderSMTLib2 Pi           = "real.pi"
  renderSMTLib2 (Sqrt x)     = renderUnary "sqrt" x
  renderSMTLib2 (Exp x)      = renderUnary "exp" x
  renderSMTLib2 (Sin x)      = renderUnary "sin" x
  renderSMTLib2 (Cos x)      = renderUnary "cos" x
  renderSMTLib2 (Tan x)      = renderUnary "tan" x
  renderSMTLib2 (Asin x)     = renderUnary "arcsin" x
  renderSMTLib2 (Acos x)     = renderUnary "arccos" x
  renderSMTLib2 (Atan x)     = renderUnary "arctan" x

  renderSMTLib2 (ToReal x)   = renderUnary "to_real" x
  renderSMTLib2 (ToInt x)    = renderUnary "to_int" x
  renderSMTLib2 (IsInt x)    = renderUnary "is_int" x

  renderSMTLib2 (Ite p t f)  = renderTernary "ite" p t f

  renderSMTLib2 (BvNot x)          = renderUnary  "bvnot"  (renderSMTLib2 x)
  renderSMTLib2 (BvAnd x y)        = renderBinary "bvand"  (renderSMTLib2 x) (renderSMTLib2 y)
  renderSMTLib2 (BvOr x y)         = renderBinary "bvor"   (renderSMTLib2 x) (renderSMTLib2 y)
  renderSMTLib2 (BvXor x y)        = renderBinary "bvxor"  (renderSMTLib2 x) (renderSMTLib2 y)
  renderSMTLib2 (BvNand x y)       = renderBinary "bvnand" (renderSMTLib2 x) (renderSMTLib2 y)
  renderSMTLib2 (BvNor x y)        = renderBinary "bvnor"  (renderSMTLib2 x) (renderSMTLib2 y)
  renderSMTLib2 (BvNeg x)          = renderUnary  "bvneg"  (renderSMTLib2 x)
  renderSMTLib2 (BvAdd x y)        = renderBinary "bvadd"  (renderSMTLib2 x) (renderSMTLib2 y)
  renderSMTLib2 (BvSub x y)        = renderBinary "bvsub"  (renderSMTLib2 x) (renderSMTLib2 y)
  renderSMTLib2 (BvMul x y)        = renderBinary "bvmul"  (renderSMTLib2 x) (renderSMTLib2 y)
  renderSMTLib2 (BvuDiv x y)       = renderBinary "bvudiv" (renderSMTLib2 x) (renderSMTLib2 y)
  renderSMTLib2 (BvuRem x y)       = renderBinary "bvurem" (renderSMTLib2 x) (renderSMTLib2 y)
  renderSMTLib2 (BvShL x y)        = renderBinary "bvshl"  (renderSMTLib2 x) (renderSMTLib2 y)
  renderSMTLib2 (BvLShR x y)       = renderBinary "bvlshr" (renderSMTLib2 x) (renderSMTLib2 y)
  renderSMTLib2 (BvConcat x y)     = renderBinary "concat" (renderSMTLib2 x) (renderSMTLib2 y)
  renderSMTLib2 (BvRotL i x)       = renderUnary (renderBinary "_" ("rotate_left"  :: Builder) (renderSMTLib2 (natVal i))) (renderSMTLib2 x)
  renderSMTLib2 (BvRotR i x)       = renderUnary (renderBinary "_" ("rotate_right" :: Builder) (renderSMTLib2 (natVal i))) (renderSMTLib2 x)
  renderSMTLib2 (BvuLT x y)        = renderBinary "bvult"  (renderSMTLib2 x) (renderSMTLib2 y)
  renderSMTLib2 (BvuLTHE x y)      = renderBinary "bvule"  (renderSMTLib2 x) (renderSMTLib2 y)
  renderSMTLib2 (BvuGTHE x y)      = renderBinary "bvuge"  (renderSMTLib2 x) (renderSMTLib2 y)
  renderSMTLib2 (BvuGT x y)        = renderBinary "bvugt"  (renderSMTLib2 x) (renderSMTLib2 y)

  renderSMTLib2 (ForAll mQvar f) = renderQuantifier "forall" mQvar f
  renderSMTLib2 (Exists mQvar f) = renderQuantifier "exists" mQvar f

renderQuantifier :: forall t. KnownSMTRepr t => Builder -> Maybe (SMTVar t) -> (Expr t -> Expr BoolType) -> Builder
renderQuantifier qname (Just qvar) f =
  renderBinary
    qname
    ("(" <> renderUnary (renderSMTLib2 qvar) (singRepr @t) <> ")")
    expr
  where
    expr = renderSMTLib2 $ f $ Var qvar
renderQuantifier _ Nothing _ = mempty
