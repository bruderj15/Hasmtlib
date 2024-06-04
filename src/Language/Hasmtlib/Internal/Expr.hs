{-# LANGUAGE TypeFamilyDependencies #-}

module Language.Hasmtlib.Internal.Expr where

import Data.Kind

-- | Types of variables in SMTLib - used as promoted Type
data SMTType = IntType | RealType | BoolType

-- | SMT variable
newtype SMTVar (t :: SMTType) = SMTVar { varId :: Int } deriving (Show, Eq, Ord)

-- | Computes the Haskell type of the SMTLib-Type
type family ValueType (t :: SMTType) = (r :: Type) | r -> t where
  ValueType IntType  = Integer
  ValueType RealType = Double
  ValueType BoolType = Bool

-- | SMT value
data Value (t :: SMTType) where
  IntValue  :: ValueType IntType  -> Value IntType
  RealValue :: ValueType RealType -> Value RealType
  BoolValue :: ValueType BoolType -> Value BoolType

extractValue :: Value t -> ValueType t
extractValue (IntValue  v) = v
extractValue (RealValue v) = v
extractValue (BoolValue v) = v

putValue :: forall t. KnownSMTRepr t => ValueType t -> Value t
putValue = case singRepr @t of
  IntRepr  -> IntValue 
  RealRepr -> RealValue
  BoolRepr -> BoolValue

deriving instance Show (Value t)
deriving instance Eq   (Value t)
deriving instance Ord  (Value t)

-- | Representation of the SMTLib-Type
data Repr (t :: SMTType) where
  IntRepr  :: Repr IntType
  RealRepr :: Repr RealType
  BoolRepr :: Repr BoolType

deriving instance Show (Repr t)
deriving instance Eq   (Repr t)
deriving instance Ord  (Repr t)

-- | Singleton for Repr t
class    KnownSMTRepr (t :: SMTType) where singRepr :: Repr t
instance KnownSMTRepr IntType        where singRepr = IntRepr
instance KnownSMTRepr RealType       where singRepr = RealRepr
instance KnownSMTRepr BoolType       where singRepr = BoolRepr

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
  Div       :: Expr RealType -> Expr RealType -> Expr RealType
  
  -- Atoms
  LTH       :: (Ord (ValueType t), KnownSMTRepr t) => Expr t -> Expr t -> Expr BoolType
  LTHE      :: (Ord (ValueType t), KnownSMTRepr t) => Expr t -> Expr t -> Expr BoolType
  EQU       :: (Eq  (ValueType t), KnownSMTRepr t) => Expr t -> Expr t -> Expr BoolType
  GTHE      :: (Ord (ValueType t), KnownSMTRepr t) => Expr t -> Expr t -> Expr BoolType
  GTH       :: (Ord (ValueType t), KnownSMTRepr t) => Expr t -> Expr t -> Expr BoolType

  -- Formulas
  Not       :: Expr BoolType -> Expr BoolType
  And       :: Expr BoolType -> Expr BoolType -> Expr BoolType
  Or        :: Expr BoolType -> Expr BoolType -> Expr BoolType
  Impl      :: Expr BoolType -> Expr BoolType -> Expr BoolType
  Xor       :: Expr BoolType -> Expr BoolType -> Expr BoolType

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

deriving instance Show (Expr t)
