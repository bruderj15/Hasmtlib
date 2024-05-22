module Language.Hasmtlib.Types.SMT where

data SMTType = IntType | RealType | BoolType

data Value (t :: SMTType) where
  IntValue  :: Integral   a => a -> Value IntType
  RealValue :: Fractional a => a -> Value RealType
  BoolValue :: Bool              -> Value BoolType  
   