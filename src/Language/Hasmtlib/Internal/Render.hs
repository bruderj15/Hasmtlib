module Language.Hasmtlib.Internal.Render where

import Data.ByteString.Builder
import GHC.TypeNats

-- | Render values to their SMTLib2-Lisp form, represented as @Builder@.
class Render a where
  render :: a -> Builder
   
instance Render Bool where
  render b = if b then "true" else "false"
  {-# INLINEABLE render #-}

instance Render Nat where
  render = integerDec . fromIntegral
  {-# INLINEABLE render #-}

instance Render Integer where
  render x
    | x < 0     = "(- " <> integerDec (abs x) <> ")"
    | otherwise = integerDec x
  {-# INLINEABLE render #-}

instance Render Double where
  render x
    | x < 0     = "(- " <> formatDouble standardDefaultPrecision (abs x) <> ")"
    | otherwise = formatDouble standardDefaultPrecision x
  {-# INLINEABLE render #-}

instance Render Builder where
  render = id
  {-# INLINEABLE render #-}

renderUnary :: Render a => Builder -> a -> Builder
renderUnary op x = "(" <> op <> " " <> render x <> ")"
{-# INLINEABLE renderUnary #-}

renderBinary :: (Render a, Render b) => Builder -> a -> b -> Builder
renderBinary op x y = "(" <> op <> " " <> render x <> " " <> render y <> ")"
{-# INLINEABLE renderBinary #-}

renderTernary :: (Render a, Render b, Render c) => Builder -> a -> b -> c -> Builder
renderTernary op x y z = "(" <> op <> " " <> render x <> " " <> render y <> " " <> render z <> ")"
{-# INLINEABLE renderTernary #-}
