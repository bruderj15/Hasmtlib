{- |
This module provides the data-type 'SMTOption' for adjusting a SMT-Solvers options.
-}
module Language.Hasmtlib.Type.Option where

import Language.Hasmtlib.Internal.Render
import Data.Data (Data)
import Data.ByteString.Builder

-- | Options for SMT-Solvers.
data SMTOption =
    PrintSuccess  Bool              -- ^ Print \"success\" after each operation
  | ProduceModels Bool              -- ^ Produce a satisfying assignment after each successful checkSat
  | Incremental   Bool              -- ^ Incremental solving
  | Custom String String            -- ^ Custom options. First String is the option, second its value.
  deriving (Show, Eq, Ord, Data)

instance Render SMTOption where
  render (PrintSuccess  b) = renderBinary "set-option" (":print-success"  :: Builder) b
  render (ProduceModels b) = renderBinary "set-option" (":produce-models" :: Builder) b
  render (Incremental   b) = renderBinary "set-option" (":incremental"    :: Builder) b
  render (Custom k v)      = renderBinary "set-option" (":" <> render k) (render v)
