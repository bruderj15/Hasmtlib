module Language.Hasmtlib.Types.Expr where

import Language.Hasmtlib.Types.SMT

data Expr (t :: SMTType) where
  Var      :: Int -> Expr t
  Constant :: Value t -> Expr t

  Plus     :: Expr t -> Expr t -> Expr t
  Neg      :: Expr t -> Expr t
  Mul      :: Expr t -> Expr t -> Expr t
  Div      :: Expr RealType -> Expr RealType -> Expr RealType
  Sqrt     :: Expr RealType -> Expr RealType

  Pi       :: Expr RealType
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

instance Num (Expr IntType) where
  fromInteger = Constant . IntValue
  (+)         = Plus
  x - y       = Plus (x) (Neg y)
  (*)         = Mul
  negate      = Neg
  abs         = error "abs not yet implemented"
  signum      = error "signum not yet implemented"

instance Num (Expr RealType) where
  fromInteger = Constant . RealValue . fromIntegral
  (+)         = Plus
  x - y       = Plus (x) (Neg y)
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
