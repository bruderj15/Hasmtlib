module Language.Hasmtlib.Type.Option where

import Language.Hasmtlib.Internal.Render
import Data.Data (Data)
import Data.ByteString.Builder
  
data SMTOption =
    PrintSuccess  Bool              -- | Print \"success\" after each operation
  | ProduceModels Bool              -- | Produce a satisfying assignment after each successful checkSat
  | Incremental   Bool              -- | Incremental solving
  deriving (Show, Eq, Ord, Data)

instance RenderSMTLib2 SMTOption where
  renderSMTLib2 (PrintSuccess  b) = renderBinary "set-option" (":print-success"  :: Builder) b
  renderSMTLib2 (ProduceModels b) = renderBinary "set-option" (":produce-models" :: Builder) b
  renderSMTLib2 (Incremental   b) = renderBinary "set-option" (":incremental"    :: Builder) b