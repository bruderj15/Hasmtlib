module Language.Hasmtlib.Type.Expr where

import Language.Hasmtlib.Type.SMT
import Language.Hasmtlib.Boolean
import Data.Foldable (foldr')

data Expr (t :: SMTType) where
  Var      :: SMTVar t -> Expr t
  Constant :: Value t  -> Expr t

  -- Terms
  Plus     :: Expr t -> Expr t -> Expr t
  Neg      :: Expr t -> Expr t
  Mul      :: Expr t -> Expr t -> Expr t
  Div      :: Expr RealType -> Expr RealType -> Expr RealType
  
  -- Atoms
  LTH       :: Expr t -> Expr t -> Expr BoolType
  LTHE      :: Expr t -> Expr t -> Expr BoolType
  EQU       :: Expr t -> Expr t -> Expr BoolType
  GTHE      :: Expr t -> Expr t -> Expr BoolType
  GTH       :: Expr t -> Expr t -> Expr BoolType

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

deriving instance Show (Expr t)  

instance Num (Expr IntType) where
  fromInteger = Constant . IntValue
  (+)         = Plus
  x - y       = Plus x (Neg y)
  (*)         = Mul
  negate      = Neg
  abs         = error "abs not yet implemented"
  signum      = error "signum not yet implemented"

instance Num (Expr RealType) where
  fromInteger = Constant . RealValue . fromIntegral
  (+)         = Plus
  x - y       = Plus x (Neg y)
  (*)         = Mul
  negate      = Neg
  abs         = error "abs not yet implemented"
  signum      = error "signum not yet implemented"

instance Fractional (Expr RealType) where
  fromRational = Constant . RealValue
  (/)          = Div

instance Floating (Expr RealType) where
    pi = Pi
    exp = Exp
    log = Log
    sqrt = Sqrt
    sin = Sin
    cos = Cos
    tan = Tan
    asin = Asin
    acos = Acos
    atan = Atan
    sinh = Sinh
    cosh = Cosh
    tanh = Tanh
    asinh = Asinh
    acosh = Acosh
    atanh = Atanh

instance Boolean (Expr BoolType) where
  bool    = Constant . BoolValue
  (&&&)   = Mul
  (|||)   = Plus
  not'    = Neg  
  all' p  = foldr' (\expr acc -> acc &&& p expr) true
  any' p  = not' . all' (not' . p)
  xor x y = Neg $ EQU x y