{-# LANGUAGE OverloadedStrings #-}

module Language.Hasmtlib.Solver where
 
import Language.Hasmtlib.Type.SMT
import Language.Hasmtlib.Type.Expr
import Data.ByteString.Builder
import Data.ByteString.Lazy hiding (foldl')
import Data.Foldable (foldl')
import Data.AttoLisp
import Data.Sequence

-- TODO: Use foldb :: b -> (a -> b) -> (b -> b -> b) -> Seq a -> b? Bench!
buildSMT :: SMT -> ByteString
buildSMT (SMT _ ivs rvs bvs fs ml) =
  toLazyByteString $
     maybe mempty stringUtf8 ml
  <> charUtf8 '\n'
  <> buildVars ivs
  <> charUtf8 '\n'
  <> buildVars rvs
  <> charUtf8 '\n'
  <> buildVars bvs
  <> charUtf8 '\n'
  <> foldl' (\s f -> s <> charUtf8 '\n' <> fromLispExpr (List [Symbol "assert", toLisp f])) mempty fs
  <> charUtf8 '\n'

buildVars :: forall t. KnownSMTRepr t => Seq (SMTVar t) -> Builder
buildVars = foldl'
  (\s v ->
       s
    <> charUtf8 '\n'
    <> fromLispExpr (List [Symbol "declare-fun", toLisp v, Symbol "()", smtType]))
  mempty
    where smtType = toLisp $ singRepr @t
