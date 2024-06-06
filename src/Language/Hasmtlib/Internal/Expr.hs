{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE RoleAnnotations #-}

module Language.Hasmtlib.Internal.Expr where

import Language.Hasmtlib.Internal.Bitvec
import Language.Hasmtlib.Boolean
import Data.Kind
import Data.Proxy
import GHC.TypeNats

-- | Types of variables in SMTLib - used as promoted Type
data SMTType = IntType | RealType | BoolType | BvType Nat

-- | SMT variable
type role SMTVar phantom
newtype SMTVar (t :: SMTType) = SMTVar { varId :: Int } deriving (Show, Eq, Ord)

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

putValue :: forall t. KnownSMTRepr t => ValueType t -> Value t
putValue = case singRepr @t of
  IntRepr  -> IntValue 
  RealRepr -> RealValue
  BoolRepr -> BoolValue
  BvRepr _ -> BvValue

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
  Log      :: Expr RealType -> Expr RealType
  Sin      :: Expr RealType -> Expr RealType
  Cos      :: Expr RealType -> Expr RealType
  Tan      :: Expr RealType -> Expr RealType
  Asin     :: Expr RealType -> Expr RealType
  Acos     :: Expr RealType -> Expr RealType
  Atan     :: Expr RealType -> Expr RealType
  Sinh     :: Expr RealType -> Expr RealType
  Cosh     :: Expr RealType -> Expr RealType
  Tanh     :: Expr RealType -> Expr RealType
  Asinh    :: Expr RealType -> Expr RealType
  Acosh    :: Expr RealType -> Expr RealType
  Atanh    :: Expr RealType -> Expr RealType

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

deriving instance Show (Expr t)

instance Boolean (Expr BoolType) where
  bool    = Constant . BoolValue
  (&&&)   = And
  (|||)   = Or
  not'    = Not
  xor     = Xor
  
instance KnownNat n => Boolean (Expr (BvType n)) where
  bool    = Constant . BvValue . bool
  (&&&)   = BvAnd
  (|||)   = BvOr
  not'    = BvNot
  xor     = BvXor
  
instance Bounded (Expr BoolType) where
  minBound = false
  maxBound = true
  
instance KnownNat n => Bounded (Expr (BvType n)) where
  minBound = Constant $ BvValue minBound
  maxBound = Constant $ BvValue maxBound