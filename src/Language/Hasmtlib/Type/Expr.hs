module Language.Hasmtlib.Type.Expr where

newtype SMTVar (t :: SMTType) = SMTVar { varId :: Int } deriving (Show, Eq, Ord)

-- Usage as DataKinds
data SMTType = IntType | RealType | BoolType

class SMTNumber (t :: SMTType)
instance SMTNumber RealType
instance SMTNumber IntType

data Value (t :: SMTType) where
  IntValue  :: Integer  -> Value IntType
  RealValue :: Rational -> Value RealType
  BoolValue :: Bool     -> Value BoolType

deriving instance Show (Value t)
deriving instance Eq (Value t)

data Expr (t :: SMTType) where
  Var      :: SMTVar t -> Expr t
  Constant :: Value t  -> Expr t

  -- Terms
  Plus     :: Expr t -> Expr t -> Expr t
  Neg      :: Expr t -> Expr t
  Mul      :: Expr t -> Expr t -> Expr t
  Abs      :: Expr t -> Expr t 
  Mod      :: Expr IntType  -> Expr IntType  -> Expr IntType 
  Div      :: Expr RealType -> Expr RealType -> Expr RealType
  
  -- Atoms
  LTH       :: Expr t -> Expr t -> Expr BoolType
  LTHE      :: Expr t -> Expr t -> Expr BoolType
  EQU       :: Expr t -> Expr t -> Expr BoolType
  GTHE      :: Expr t -> Expr t -> Expr BoolType
  GTH       :: Expr t -> Expr t -> Expr BoolType

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

deriving instance Show (Expr t)  

instance Num (Expr IntType) where
  fromInteger = Constant . IntValue
  (+)         = Plus
  x - y       = Plus x (Neg y)
  (*)         = Mul
  negate      = Neg
  abs         = Abs
  signum      = error "signum not yet implemented"

instance Num (Expr RealType) where
  fromInteger = Constant . RealValue . fromIntegral
  (+)         = Plus
  x - y       = Plus x (Neg y)
  (*)         = Mul
  negate      = Neg
  abs         = Abs
  signum      = error "signum not yet implemented"

instance Fractional (Expr RealType) where
  fromRational = Constant . RealValue
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
    