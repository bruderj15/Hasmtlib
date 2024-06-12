module Language.Hasmtlib.Internal.Render where

import Data.ByteString.Builder
import GHC.TypeNats

class RenderSMTLib2 a where
  renderSMTLib2 :: a -> Builder
   
instance RenderSMTLib2 Bool where
  renderSMTLib2 b = if b then "true" else "false"
  {-# INLINEABLE renderSMTLib2 #-}

instance RenderSMTLib2 Nat where
  renderSMTLib2 = integerDec . fromIntegral
  {-# INLINEABLE renderSMTLib2 #-}

instance RenderSMTLib2 Integer where
  renderSMTLib2 x
    | x < 0     = "(- " <> integerDec (abs x) <> ")"
    | otherwise = integerDec x
  {-# INLINEABLE renderSMTLib2 #-}

instance RenderSMTLib2 Double where
  renderSMTLib2 x
    | x < 0     = "(- " <> formatDouble standardDefaultPrecision (abs x) <> ")"
    | otherwise = formatDouble standardDefaultPrecision x
  {-# INLINEABLE renderSMTLib2 #-}

instance RenderSMTLib2 Builder where
  renderSMTLib2 = id
  {-# INLINEABLE renderSMTLib2 #-}

renderUnary :: RenderSMTLib2 a => Builder -> a -> Builder
renderUnary op x = "(" <> op <> " " <> renderSMTLib2 x <> ")"
{-# INLINEABLE renderUnary #-}

renderBinary :: (RenderSMTLib2 a, RenderSMTLib2 b) => Builder -> a -> b -> Builder
renderBinary op x y = "(" <> op <> " " <> renderSMTLib2 x <> " " <> renderSMTLib2 y <> ")"
{-# INLINEABLE renderBinary #-}

renderTernary :: (RenderSMTLib2 a, RenderSMTLib2 b, RenderSMTLib2 c) => Builder -> a -> b -> c -> Builder
renderTernary op x y z = "(" <> op <> " " <> renderSMTLib2 x <> " " <> renderSMTLib2 y <> " " <> renderSMTLib2 z <> ")"
{-# INLINEABLE renderTernary #-}
