{-# LANGUAGE LambdaCase #-}

module Language.Hasmtlib.Internal.Render where

import Language.Hasmtlib.Type.OMT
import Language.Hasmtlib.Type.SMT
import Language.Hasmtlib.Type.Expr
import Language.Hasmtlib.Type.Value
import Language.Hasmtlib.Type.Option
import Language.Hasmtlib.Type.SMTSort
import Language.Hasmtlib.Type.Bitvec
import Language.Hasmtlib.Type.ArrayMap
import Data.Coerce
import Data.Sequence
import Data.Foldable (foldl')
import Data.Map (size, minViewWithKey)
import Data.ByteString.Builder
import Data.ByteString.Lazy.UTF8 (toString)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Enc
import qualified Data.Vector.Sized as V
import Control.Lens hiding (op)
import GHC.TypeNats

render1 :: Render a => Builder -> a -> Builder
render1 op x = "(" <> op <> " " <> render x <> ")"
{-# INLINE render1 #-}

render2 :: (Render a, Render b) => Builder -> a -> b -> Builder
render2 op x y = "(" <> op <> " " <> render x <> " " <> render y <> ")"
{-# INLINE render2 #-}

render3 :: (Render a, Render b, Render c) => Builder -> a -> b -> c -> Builder
render3 op x y z = "(" <> op <> " " <> render x <> " " <> render y <> " " <> render z <> ")"
{-# INLINE render3 #-}

renderN :: Render a => Builder -> [a] -> Builder
renderN op xs = "(" <> op <> renderedXs <> ")"
  where
    renderedXs = foldl' (\s x -> s <> " " <> render x) mempty xs
{-# INLINE renderN #-}

-- | Render values to their SMTLib2-Lisp form, represented as 'Builder'.
class Render a where
  render :: a -> Builder

instance Render Bool where
  render b = if b then "true" else "false"
  {-# INLINE render #-}

instance Render Nat where
  render = integerDec . fromIntegral
  {-# INLINE render #-}

instance Render Integer where
  render x
    | x < 0     = "(- " <> integerDec (abs x) <> ")"
    | otherwise = integerDec x
  {-# INLINE render #-}

instance Render Double where
  render x
    | x < 0     = "(- " <> formatDouble standardDefaultPrecision (abs x) <> ")"
    | otherwise = formatDouble standardDefaultPrecision x
  {-# INLINE render #-}

instance Render Char where
  render = char8
  {-# INLINE render #-}

instance Render String where
  render = string8
  {-# INLINE render #-}

instance Render Builder where
  render = id
  {-# INLINE render #-}

instance Render Text.Text where
  render = Text.Enc.encodeUtf8Builder
  {-# INLINE render #-}

instance Render (Bitvec enc n) where
  render = stringUtf8 . show
  {-# INLINE render #-}

instance Render (SMTVar t) where
  render v = "var_" <> intDec (coerce @(SMTVar t) @Int v)
  {-# INLINE render #-}

instance Show (Value t) where
  show = toString . toLazyByteString . render

instance Render (Value t) where
  render (IntValue x)   = render x
  render (RealValue x)  = render x
  render (BoolValue x)  = render x
  render (BvValue   v)  = "#b" <> render v
  render (ArrayValue arr) = case minViewWithKey (arr^.stored) of
    Nothing -> constRender $ arr^.arrConst
    Just ((k,v), stored')
      | size (arr^.stored) > 1 -> render $ ArrStore (Constant (wrapValue (arr & stored .~ stored'))) (Constant (wrapValue k)) (Constant (wrapValue v))
      | otherwise  -> constRender v
    where
      constRender v = "((as const " <> render (goSing arr) <> ") " <> render (wrapValue v) <> ")"
      goSing :: forall k v. (KnownSMTSort k, KnownSMTSort v, Ord (HaskellType k), Ord (HaskellType v)) => ConstArray (HaskellType k) (HaskellType v) -> SSMTSort (ArraySort k v)
      goSing _ = sortSing @(ArraySort k v)
  render (StringValue x) = "\"" <> render x <> "\""

instance KnownSMTSort t => Show (Expr t) where
  show = toString . toLazyByteString . render

instance KnownSMTSort t => Render (Expr t) where
  render (Var v)      = render v
  render (Constant c) = render c
  render (Plus x y)   = render2 (case sortSing' x of SBvSort _ _ -> "bvadd" ; _ -> "+") x y
  render (Minus x y)  = render2 (case sortSing' x of SBvSort _ _ -> "bvsub" ; _ -> "-") x y
  render (Neg x)      = render1  (case sortSing' x of SBvSort _ _ -> "bvneg" ; _ -> "-") x
  render (Mul x y)    = render2 (case sortSing' x of SBvSort _ _ -> "bvmul" ; _ -> "*") x y
  render (Abs x)      = render1  "abs" x
  render (Mod x y)    = render2 opStr x y
    where
      opStr = case sortSing' x of
        SBvSort enc _ -> case bvEncSing' enc of
          SUnsigned -> "bvurem"
          SSigned -> "bvsmod"
        _ -> "mod"
  render (Rem x y)    = render2 opStr x y
    where
      opStr = case sortSing' x of
        SBvSort enc _ -> case bvEncSing' enc of
          SUnsigned -> "bvurem"
          SSigned -> "bvsrem"
        _ -> "rem"
  render (IDiv x y)   = render2 opStr x y
    where
      opStr = case sortSing' x of
        SBvSort enc _ -> case bvEncSing' enc of
          SUnsigned -> "bvudiv"
          SSigned -> "bvsdiv"
        _ -> "div"
  render (Div x y)    = render2 "/" x y
  render (LTH x y)    = render2 opStr x y
    where
      opStr = case sortSing' x of
        SBvSort enc _ -> case bvEncSing' enc of
          SUnsigned -> "bvult"
          SSigned -> "bvslt"
        SStringSort -> "str.<"
        _ -> "<"
  render (LTHE x y)   = render2 opStr x y
    where
      opStr = case sortSing' x of
        SBvSort enc _ -> case bvEncSing' enc of
          SUnsigned -> "bvule"
          SSigned -> "bvsle"
        SStringSort -> "str.<="
        _ -> "<="
  render (EQU xs)     = renderN "=" $ V.toList xs
  render (Distinct xs)= renderN "distinct" $ V.toList xs
  render (GTHE x y)   = case sortSing' x of
    SBvSort enc _ -> case bvEncSing' enc of
      SUnsigned -> render2 "bvuge" x y
      SSigned   -> render2 "bvsge" x y
    SStringSort -> render2 "str.<=" y x
    _           -> render2 ">=" x y
  render (GTH x y)    = case sortSing' x of
    SBvSort enc _ -> case bvEncSing' enc of
      SUnsigned -> render2 "bvugt" x y
      SSigned   -> render2 "bvsgt" x y
    SStringSort -> render2 "str.<" y x
    _           -> render2 ">" x y
  render (Not x)      = render1  (case sortSing' x of SBvSort _ _ -> "bvnot" ; _ -> "not") x
  render (And x y)    = render2 (case sortSing' x of SBvSort _ _ -> "bvand" ; _ -> "and") x y
  render (Or x y)     = render2 (case sortSing' x of SBvSort _ _ -> "bvor" ; _ -> "or") x y
  render (Impl x y)   = render2 "=>" x y
  render (Xor x y)    = render2 (case sortSing' x of SBvSort _ _ -> "bvxor" ; _ -> "xor") x y
  render Pi           = "real.pi"
  render (Sqrt x)     = render1 "sqrt" x
  render (Exp x)      = render1 "exp" x
  render (Sin x)      = render1 "sin" x
  render (Cos x)      = render1 "cos" x
  render (Tan x)      = render1 "tan" x
  render (Asin x)     = render1 "arcsin" x
  render (Acos x)     = render1 "arccos" x
  render (Atan x)     = render1 "arctan" x
  render (ToReal x)   = render1 "to_real" x
  render (ToInt x)    = render1 "to_int" x
  render (IsInt x)    = render1 "is_int" x
  render (Ite p t f)  = render3 "ite" p t f
  render (BvNand x y)       = render2 "bvnand" (render x) (render y)
  render (BvNor x y)        = render2 "bvnor"  (render x) (render y)
  render (BvShL x y)        = render2 "bvshl"  (render x) (render y)
  render (BvLShR x y)       = render2 "bvlshr" (render x) (render y)
  render (BvAShR x y)       = render2 "bvashr" (render x) (render y)
  render (BvConcat x y)     = render2 "concat" (render x) (render y)
  render (BvRotL i x)       = render1 (render2 "_" ("rotate_left"  :: Builder) (render $ toInteger i)) (render x)
  render (BvRotR i x)       = render1 (render2 "_" ("rotate_right" :: Builder) (render $ toInteger i)) (render x)
  render (ArrSelect a i)    = render2  "select" (render a) (render i)
  render (ArrStore a i v)   = render3 "store"  (render a) (render i) (render v)
  render (StrConcat x y)        = render2 "str.++"  (render x) (render y)
  render (StrLength x)          = render1  "str.len" (render x)
  render (StrAt x i)            = render2 "str.at"  (render x) (render i)
  render (StrSubstring x i j)   = render3 "str.substr"  (render x) (render i) (render j)
  render (StrPrefixOf x y)      = render2 "str.prefixof" (render x) (render y)
  render (StrSuffixOf x y)      = render2 "str.suffixof" (render x) (render y)
  render (StrContains x y)      = render2 "str.contains" (render x) (render y)
  render (StrIndexOf x y i)     = render3 "str.indexof"     (render x) (render y) (render i)
  render (StrReplace x y y')    = render3 "str.replace"     (render x) (render y) (render y')
  render (StrReplaceAll x y y') = render3 "str.replace_all" (render x) (render y) (render y')
  render (ForAll mQvar f) = renderQuantifier "forall" mQvar f
  render (Exists mQvar f) = renderQuantifier "exists" mQvar f
  render (Let Nothing _ _) = mempty
  render (Let (Just l) t expr) = "(let ((" <> render l <> render t <> ")) " <> render (expr (Var l)) <> ")"

renderQuantifier :: forall t. KnownSMTSort t => Builder -> Maybe (SMTVar t) -> (Expr t -> Expr BoolSort) -> Builder
renderQuantifier qname (Just qvar) f =
  render2
    qname
    ("(" <> render1 (render qvar) (sortSing @t) <> ")")
    expr
  where
    expr = render $ f $ Var qvar
renderQuantifier _ Nothing _ = mempty

instance Render (SSMTSort t) where
  render SBoolSort   = "Bool"
  render SIntSort    = "Int"
  render SRealSort   = "Real"
  render (SBvSort _ p) = render2 "_" ("BitVec" :: Builder) (natVal p)
  render (SArraySort k v) = render2 "Array" (sortSing' k) (sortSing' v)
  render SStringSort   = "String"
  {-# INLINE render #-}

instance Render SMTOption where
  render (PrintSuccess  b) = render2 "set-option" (":print-success"  :: Builder) b
  render (ProduceModels b) = render2 "set-option" (":produce-models" :: Builder) b
  render (Incremental   b) = render2 "set-option" (":incremental"    :: Builder) b
  render (Custom k v)      = render2 "set-option" (":" <> render k) (render v)

instance Render SoftFormula where
  render sf = "(assert-soft " <> render (sf^.formula) <> " :weight " <> maybe "1" render (sf^.mWeight) <> renderGroupId (sf^.mGroupId) <> ")"
    where
      renderGroupId Nothing = mempty
      renderGroupId (Just groupId) = " :id " <> render groupId

instance KnownSMTSort t => Render (Minimize t) where
  render (Minimize expr) = "(minimize " <> render expr <> ")"

instance KnownSMTSort t => Render (Maximize t) where
  render (Maximize expr) = "(maximize " <> render expr <> ")"

renderSetLogic :: Builder -> Builder
renderSetLogic = render1 "set-logic"
{-# INLINE renderSetLogic #-}

renderDeclareVar :: forall t. KnownSMTSort t => SMTVar t -> Builder
renderDeclareVar v = render3 "declare-fun" v ("()" :: Builder) (sortSing @t)
{-# INLINE renderDeclareVar #-}

renderAssert :: Expr BoolSort -> Builder
renderAssert = render1 "assert"
{-# INLINE renderAssert #-}

renderGetValue :: SMTVar t -> Builder
renderGetValue x = render1 "get-value" $ "(" <> render x <> ")"
{-# INLINE renderGetValue #-}

renderPush :: Integer -> Builder
renderPush i = "(push "<> render i <>")"
{-# INLINE renderPush #-}

renderPop :: Integer -> Builder
renderPop i = "(pop "<> render i <>")"
{-# INLINE renderPop #-}

renderCheckSat :: Builder
renderCheckSat = "(check-sat)"
{-# INLINE renderCheckSat #-}

renderGetModel :: Builder
renderGetModel = "(get-model)"
{-# INLINE renderGetModel #-}

class RenderProblem s where
  renderOptions        :: s -> Seq Builder
  renderLogic          :: s -> Builder
  renderDeclareVars    :: s -> Seq Builder
  renderAssertions     :: s -> Seq Builder
  renderSoftAssertions :: s -> Seq Builder
  renderMinimizations  :: s -> Seq Builder
  renderMaximizations  :: s -> Seq Builder

instance RenderProblem SMT where
  renderOptions = fromList . fmap render . view options
  renderLogic = maybe mempty (renderSetLogic . stringUtf8) . view mlogic
  renderDeclareVars = fmap (\(SomeSMTSort v) -> renderDeclareVar v) . view vars
  renderAssertions = fmap renderAssert . view formulas
  renderSoftAssertions _ = mempty
  renderMinimizations _ = mempty
  renderMaximizations _ = mempty

instance RenderProblem OMT where
  renderOptions = renderOptions . view smt
  renderLogic = renderLogic . view smt
  renderDeclareVars = renderDeclareVars . view smt
  renderAssertions = renderAssertions . view smt
  renderSoftAssertions = fmap render . view softFormulas
  renderMinimizations = fmap (\case SomeSMTSort minExpr -> render minExpr) . view targetMinimize
  renderMaximizations = fmap (\case SomeSMTSort maxExpr -> render maxExpr) . view targetMaximize
