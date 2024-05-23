module Language.Hasmtlib.Type.SMT where

newtype SMTVar (t :: SMTType) = SMTVar { varId :: Int } deriving (Show, Eq, Ord)

-- Usage as DataKinds  
data SMTType = IntType | RealType | BoolType

data Value (t :: SMTType) where
  IntValue  :: Integer  -> Value IntType
  RealValue :: Rational -> Value RealType
  BoolValue :: Bool     -> Value BoolType
  
deriving instance Show (Value t)  
deriving instance Eq (Value t)  
   