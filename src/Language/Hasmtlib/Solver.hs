{-# LANGUAGE OverloadedStrings #-}

module Language.Hasmtlib.Solver where
 
import Language.Hasmtlib.Type.SMT
import Language.Hasmtlib.Type.Expr
import Control.Lens hiding (List)
import Data.ByteString.Builder
import Data.ByteString.Lazy hiding (foldl')
import Data.Foldable (foldl')
import Data.AttoLisp
import Data.Sequence

-- TODO: Use foldb :: b -> (a -> b) -> (b -> b -> b) -> Seq a -> b? Bench!
buildSMT :: SMT -> ByteString
buildSMT smt =
  toLazyByteString $
     maybe mempty stringUtf8 (smt^.mlogic)
  <> charUtf8 '\n'
  <> buildVars (smt^.vars)
  <> charUtf8 '\n'
  <> foldl' (\s f -> s <> charUtf8 '\n' <> fromLispExpr (List [Symbol "assert", toLisp f])) mempty (smt^.formulas)
  <> charUtf8 '\n'

buildVars :: Seq (SomeKnownSMTRepr SMTVar) -> Builder
buildVars = foldl'
  (\s (SomeKnownSMTRepr v@(SMTVar _ mval)) ->
       s
    <> charUtf8 '\n'
    <> fromLispExpr (List [Symbol "declare-fun", toLisp v, Symbol "()", goSing mval]))
  mempty
    where
      goSing :: forall t. KnownSMTRepr t => Maybe (Value t) -> Lisp
      goSing _ = toLisp $ singRepr @t

