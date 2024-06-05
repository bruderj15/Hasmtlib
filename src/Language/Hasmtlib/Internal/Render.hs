module Language.Hasmtlib.Internal.Render where

import Language.Hasmtlib.Internal.Expr
import Language.Hasmtlib.Type.SMT
import Data.ByteString.Builder
import Data.Sequence hiding ((|>), filter)
import Data.Coerce
import Data.Bit
import qualified Data.Vector.Unboxed.Sized as V
import Control.Lens hiding (op)
import GHC.TypeNats

class RenderSMTLib2 a where
  renderSMTLib2 :: a -> Builder

instance RenderSMTLib2 (Repr t) where
  renderSMTLib2 IntRepr    = "Int"
  renderSMTLib2 RealRepr   = "Real"
  renderSMTLib2 BoolRepr   = "Bool"
  renderSMTLib2 (BvRepr p) = renderBinary "_" ("BitVec" :: Builder) (natVal p)
  {-# INLINEABLE renderSMTLib2 #-}
   
instance RenderSMTLib2 Bool where
  renderSMTLib2 b = if b then "true" else "false"
  {-# INLINEABLE renderSMTLib2 #-}

instance RenderSMTLib2 Nat where
  renderSMTLib2 = integerDec . fromIntegral

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

instance RenderSMTLib2 (SMTVar t) where
  renderSMTLib2 v = "var_" <> intDec (coerce @(SMTVar t) @Int v)
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

instance KnownSMTRepr t => RenderSMTLib2 (Expr t) where
  renderSMTLib2 (Var v)                  = renderSMTLib2 v
  renderSMTLib2 (Constant (IntValue x))  = renderSMTLib2 x
  renderSMTLib2 (Constant (RealValue x)) = renderSMTLib2 x
  renderSMTLib2 (Constant (BoolValue x)) = renderSMTLib2 x
  renderSMTLib2 (Constant (BvValue   v)) = "#b" <> binPart
    where 
      binPart = stringUtf8 $ V.toList $ V.map (\b -> if coerce b then '1' else '0') v
  renderSMTLib2 (Plus x y)   = renderBinary "+" x y
  renderSMTLib2 (Neg x)      = renderUnary  "-" x
  renderSMTLib2 (Mul x y)    = renderBinary "*" x y
  renderSMTLib2 (Abs x)      = renderUnary  "abs" x
  renderSMTLib2 (Mod x y)    = renderBinary "mod" x y
  renderSMTLib2 (Div x y)    = renderBinary "/" x y

  renderSMTLib2 (LTH x y)    = renderBinary "<" x y
  renderSMTLib2 (LTHE x y)   = renderBinary "<=" x y
  renderSMTLib2 (EQU x y)    = renderBinary "=" x y
  renderSMTLib2 (Distinct x y) = renderBinary "distinct" x y
  renderSMTLib2 (GTHE x y)   = renderBinary ">=" x y
  renderSMTLib2 (GTH x y)    = renderBinary ">" x y

  renderSMTLib2 (Not x)      = renderUnary  "not" x
  renderSMTLib2 (And x y)    = renderBinary "and" x y
  renderSMTLib2 (Or x y)     = renderBinary "or" x y
  renderSMTLib2 (Impl x y)   = renderBinary "=>" x y
  renderSMTLib2 (Xor x y)    = renderBinary "xor" x y

  renderSMTLib2 Pi           = "real.pi"
  renderSMTLib2 (Sqrt x)     = renderUnary "sqrt" x
  renderSMTLib2 (Exp x)      = renderUnary "exp" x
  renderSMTLib2 (Log _)      = error "Language.Hasmtlib.Internal.Render#renderSMTLib2(Log x) is not yet supported by solvers."
  renderSMTLib2 (Sin x)      = renderUnary "sin" x
  renderSMTLib2 (Cos x)      = renderUnary "cos" x
  renderSMTLib2 (Tan x)      = renderUnary "tan" x
  renderSMTLib2 (Asin x)     = renderUnary "arcsin" x
  renderSMTLib2 (Acos x)     = renderUnary "arccos" x
  renderSMTLib2 (Atan x)     = renderUnary "arctan" x
  renderSMTLib2 (Sinh _)     = error "Language.Hasmtlib.Internal.Render#renderSMTLib2(Sinh x) is not yet supported by solvers."
  renderSMTLib2 (Cosh _)     = error "Language.Hasmtlib.Internal.Render#renderSMTLib2(Cosh x) is not yet supported by solvers."
  renderSMTLib2 (Tanh _)     = error "Language.Hasmtlib.Internal.Render#renderSMTLib2(Tanh x) is not yet supported by solvers."
  renderSMTLib2 (Asinh _)    = error "Language.Hasmtlib.Internal.Render#renderSMTLib2(Asinh x) is not yet supported by solvers."
  renderSMTLib2 (Acosh _)    = error "Language.Hasmtlib.Internal.Render#renderSMTLib2(Acosh x) is not yet supported by solvers."
  renderSMTLib2 (Atanh _)    = error "Language.Hasmtlib.Internal.Render#renderSMTLib2(Atanh x) is not yet supported by solvers."

  renderSMTLib2 (ToReal x)   = renderUnary "to_real" x
  renderSMTLib2 (ToInt x)    = renderUnary "to_int" x
  renderSMTLib2 (IsInt x)    = renderUnary "is_int" x

  renderSMTLib2 (Ite p t f)  = renderTernary "ite" p t f
  
  renderSMTLib2 (BvNot x)          = renderUnary  "bvnot"  (renderSMTLib2 x)
  renderSMTLib2 (BvAnd x y)        = renderBinary "bvand"  (renderSMTLib2 x) (renderSMTLib2 y)
  renderSMTLib2 (BvOr x y)         = renderBinary "bvor"   (renderSMTLib2 x) (renderSMTLib2 y)
  renderSMTLib2 (BvXor x y)        = renderBinary "bvxor"  (renderSMTLib2 x) (renderSMTLib2 y)
  renderSMTLib2 (BvNand x y)       = renderBinary "bvnand" (renderSMTLib2 x) (renderSMTLib2 y)
  renderSMTLib2 (BvNor x y)        = renderBinary "bvnor"  (renderSMTLib2 x) (renderSMTLib2 y)
  renderSMTLib2 (BvNeg x)          = renderUnary  "bvneg"  (renderSMTLib2 x)
  renderSMTLib2 (BvAdd x y)        = renderBinary "bvadd"  (renderSMTLib2 x) (renderSMTLib2 y)
  renderSMTLib2 (BvSub x y)        = renderBinary "bvsub"  (renderSMTLib2 x) (renderSMTLib2 y)
  renderSMTLib2 (BvMul x y)        = renderBinary "bvmul"  (renderSMTLib2 x) (renderSMTLib2 y)
  renderSMTLib2 (BvuDiv x y)       = renderBinary "bvudiv" (renderSMTLib2 x) (renderSMTLib2 y)
  renderSMTLib2 (BvuRem x y)       = renderBinary "bvurem" (renderSMTLib2 x) (renderSMTLib2 y)
  renderSMTLib2 (BvShL x y)        = renderBinary "bvshl"  (renderSMTLib2 x) (renderSMTLib2 y)
  -- TODO: Which of these are parametric and require different rendering due to parametricity?
  renderSMTLib2 (BvLShR x y)       = renderBinary "bvlshr" (renderSMTLib2 x) (renderSMTLib2 y)
  renderSMTLib2 (BvConcat x y)     = renderBinary "concat" (renderSMTLib2 x) (renderSMTLib2 y)
  renderSMTLib2 (BvExtract i j x)  = renderTernary "extract" (renderSMTLib2 (natVal i)) (renderSMTLib2 (natVal j)) (renderSMTLib2 x)
  renderSMTLib2 (BvZeroExtend i x) = renderBinary "zero_extend" (renderSMTLib2 (natVal i)) (renderSMTLib2 x)
  renderSMTLib2 (BvRotL i x)       = renderUnary (renderBinary "_" ("rotate_left" :: Builder) (renderSMTLib2 (natVal i))) (renderSMTLib2 x)
  renderSMTLib2 (BvRotR i x)       = renderUnary (renderBinary "_" ("rotate_right" :: Builder) (renderSMTLib2 (natVal i))) (renderSMTLib2 x)
  renderSMTLib2 (BvuLT x y)        = renderBinary "bvult"  (renderSMTLib2 x) (renderSMTLib2 y)
  renderSMTLib2 (BvuLTHE x y)      = renderBinary "bvule"  (renderSMTLib2 x) (renderSMTLib2 y)
  renderSMTLib2 (BvuGTHE x y)      = renderBinary "bvuge"  (renderSMTLib2 x) (renderSMTLib2 y)
  renderSMTLib2 (BvuGT x y)        = renderBinary "bvugt"  (renderSMTLib2 x) (renderSMTLib2 y)
  {-# INLINEABLE renderSMTLib2 #-}

instance RenderSMTLib2 SMTOption where
  renderSMTLib2 (PrintSuccess  b) = renderBinary "set-option" (":print-success" :: Builder)  b
  renderSMTLib2 (ProduceModels b) = renderBinary "set-option" (":produce-models" :: Builder) b

renderSetLogic :: Builder -> Builder
renderSetLogic = renderUnary "set-logic"

renderDeclareVar :: SMTVar t -> Repr t -> Builder
renderDeclareVar v = renderTernary "declare-fun" v ("()" :: Builder)
{-# INLINEABLE renderDeclareVar #-}

renderAssert :: Expr BoolType -> Builder
renderAssert = renderUnary "assert"
{-# INLINEABLE renderAssert #-}

renderCheckSat :: Builder
renderCheckSat = "(check-sat)"

renderGetModel :: Builder
renderGetModel = "(get-model)"

renderSMT :: SMT -> Seq Builder
renderSMT smt =
     fromList (renderSMTLib2 <$> smt^.options)
  >< maybe mempty (singleton . renderSetLogic . stringUtf8) (smt^.mlogic)
  >< renderVars (smt^.vars)
  >< fmap renderAssert (smt^.formulas)

renderVars :: Seq (SomeKnownSMTRepr SMTVar) -> Seq Builder
renderVars = fmap (\(SomeKnownSMTRepr v) -> renderDeclareVar v (goSing v))
  where
    goSing :: forall t. KnownSMTRepr t => SMTVar t -> Repr t
    goSing _ = singRepr @t