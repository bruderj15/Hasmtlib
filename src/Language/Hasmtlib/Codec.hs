module Language.Hasmtlib.Codec where

import Prelude hiding (lookup)
import Data.Kind
import Control.Monad
import Language.Hasmtlib.Type.Expr 
import Data.ByteString.Lazy
import Data.Sequence

type Solution = ByteString

class Codec a where 
  type Decoded a :: Type
  decode :: Solution -> a -> Decoded a
  
--instance Codec (SMTVar t) t where
--  type Decoded (SMTVar t) = ValueType t
--  decode sol var = fmap extractValue $ join $ lookup var sol
