-- Required for class constraints of form: c (ValueType t) :: Constraint
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Language.Hasmtlib.Internal.Expr where

import Language.Hasmtlib.Iteable
import Language.Hasmtlib.Boolean
import Language.Hasmtlib.Equatable
import Language.Hasmtlib.Orderable
import Data.AttoLisp
import Data.Text hiding (foldl')
import Data.Coerce
import Data.Foldable (foldl')
import Data.Kind

-- | Types of variables in SMTLib - used as promoted Type
data SMTType = IntType | RealType | BoolType

-- | SMT variable
newtype SMTVar (t :: SMTType) = SMTVar { varId :: Int } deriving (Show, Eq, Ord)

-- | Computes the Haskell representation of the SMTLib-Type
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

class    SMTNumber (t :: SMTType) where smtValueFromInteger :: Integer -> Value t
instance SMTNumber RealType       where smtValueFromInteger = RealValue . fromIntegral
instance SMTNumber IntType        where smtValueFromInteger = IntValue

instance Iteable (Expr BoolType) (Expr t) where
  ite = Ite

instance Boolean (Expr BoolType) where
  bool    = Constant . BoolValue
  (&&&)   = And
  (|||)   = Or
  not'    = Not
  all' p  = foldl' (\acc expr -> acc &&& p expr) true
  any' p  = not' . all' (not' . p)
  xor     = Xor

instance (KnownSMTRepr t, Eq (ValueType t)) => Equatable (Expr t) where
  type EqResult (Expr t) = Expr BoolType
  (===)   = EQU
  x /== y = Not $ EQU x y

instance (KnownSMTRepr t, Ord (ValueType t)) => Orderable (Expr t) where
  type OrdResult (Expr t) = Expr BoolType
  (<?)    = LTH
  (<=?)   = LTHE
  (>=?)   = GTHE
  (>?)    = GTH
  x -? y  = ite (x <? y) x y
  x +? y  = ite (x >? y) x y

instance (KnownSMTRepr t, SMTNumber t, Num (ValueType t), Ord (ValueType t)) => Num (Expr t) where
  fromInteger = Constant . smtValueFromInteger
  (+)         = Plus
  x - y       = Plus x (Neg y)
  (*)         = Mul
  negate      = Neg
  abs         = Abs
  signum x    = ite (x === 0) 0 $ ite (x <? 0) (-1) 1

instance Fractional (Expr RealType) where
  fromRational = Constant . RealValue . fromRational
  (/)          = Div

instance Floating (Expr RealType) where
    pi    = Pi
    exp   = Exp
    log   = Log
    sqrt  = Sqrt
    sin   = Sin
    cos   = Cos
    tan   = Tan
    asin  = Asin
    acos  = Acos
    atan  = Atan
    sinh  = Sinh
    cosh  = Cosh
    tanh  = Tanh
    asinh = Asinh
    acosh = Acosh
    atanh = Atanh

instance ToLisp (Repr t) where
   toLisp IntRepr  = Symbol "Int"
   toLisp RealRepr = Symbol "Real"
   toLisp BoolRepr = Symbol "Bool"

-- TODO: Can we avoid the intermediate String here?
instance ToLisp (SMTVar t) where
  toLisp v = Symbol $ "var_" <> pack (show (coerce @(SMTVar t) @Int v))
    
-- Some of these are backend-dependant
-- Adjust in future
instance KnownSMTRepr t => ToLisp (Expr t) where
  toLisp (Var v)                  = toLisp v
  toLisp (Constant (BoolValue v)) = Symbol $ if v then "true" else "false"
  toLisp (Constant (IntValue  v)) = toLisp v
  toLisp (Constant (RealValue v)) = toLisp v

  toLisp (Plus x y)   = List [Symbol "+",   toLisp x, toLisp y]
  toLisp (Neg x)      = List [Symbol "-",   toLisp x]
  toLisp (Mul x y)    = List [Symbol "*",   toLisp x, toLisp y]
  toLisp (Abs x)      = List [Symbol "abs", toLisp x]
  toLisp (Mod x y)    = List [Symbol "mod", toLisp x, toLisp y]
  toLisp (Div x y)    = List [Symbol "/",   toLisp x, toLisp y]

  toLisp (LTH x y)    = List [Symbol "<",   toLisp x, toLisp y]
  toLisp (LTHE x y)   = List [Symbol "<=",  toLisp x, toLisp y]
  toLisp (EQU x y)    = List [Symbol "=",   toLisp x, toLisp y]
  toLisp (GTHE x y)   = List [Symbol ">=",  toLisp x, toLisp y]
  toLisp (GTH x y)    = List [Symbol ">",   toLisp x, toLisp y]

  toLisp (Not x)      = List [Symbol "not", toLisp x]
  toLisp (And x y)    = List [Symbol "and", toLisp x, toLisp y]
  toLisp (Or x y)     = List [Symbol "or",  toLisp x, toLisp y]
  toLisp (Impl x y)   = List [Symbol "=>",  toLisp x, toLisp y]
  toLisp (Xor x y)    = List [Symbol "xor", toLisp x, toLisp y]

  -- TODO: Replace ??? with actual ones
  toLisp Pi           = Symbol "real.pi"
  toLisp (Sqrt x)     = List [Symbol "sqrt",    toLisp x]
  toLisp (Exp x)      = List [Symbol "exp",     toLisp x]
--  toLisp (Log x)      = List [Symbol "???",     toLisp x]
  toLisp (Sin x)      = List [Symbol "sin",     toLisp x]
  toLisp (Cos x)      = List [Symbol "cos",     toLisp x]
  toLisp (Tan x)      = List [Symbol "tan",     toLisp x]
  toLisp (Asin x)     = List [Symbol "arcsin",  toLisp x]
  toLisp (Acos x)     = List [Symbol "arccos",  toLisp x]
  toLisp (Atan x)     = List [Symbol "arctan",  toLisp x]
--  toLisp (Sinh x)     = List [Symbol "???",     toLisp x]
--  toLisp (Cosh x)     = List [Symbol "???",     toLisp x]
--  toLisp (Tanh x)     = List [Symbol "???",     toLisp x]
--  toLisp (Asinh x)    = List [Symbol "???",     toLisp x]
--  toLisp (Acosh x)    = List [Symbol "???",     toLisp x]
--  toLisp (Atanh x)    = List [Symbol "???",     toLisp x]

  toLisp (ToReal x)   = List [Symbol "to_real", toLisp x]
  toLisp (ToInt x)    = List [Symbol "to_int",  toLisp x]
  toLisp (IsInt x)    = List [Symbol "is_int",  toLisp x]

  toLisp (Ite p t f)  = List [Symbol "ite", toLisp p, toLisp t, toLisp f]
