{-# LANGUAGE OverloadedStrings #-}

module Language.Hasmtlib.Problem where

import Language.Hasmtlib.Type.SMT
import Language.Hasmtlib.Type.Expr
import Data.Foldable (foldl')
import Data.AttoLisp
import Data.ByteString.Builder
import Data.ByteString.Lazy hiding (foldl', filter)
import Data.Sequence hiding ((|>), filter)
import Control.Lens hiding (List)
import qualified Data.Text as T
  
  
-- TODO: Use foldb :: b -> (a -> b) -> (b -> b -> b) -> Seq a -> b? Bench!
buildSMT :: SMT -> ByteString
buildSMT smt =
  toLazyByteString $
     foldl' (\s opt -> s <> charUtf8 '\n' <> fromLispExpr (toLisp opt)) mempty (smt^.options)
  <> charUtf8 '\n'
  <> maybe mempty (\l -> fromLispExpr (List [Symbol "set-logic", Symbol (T.pack l)])) (smt^.mlogic)
  <> charUtf8 '\n'
  <> buildVars (smt^.vars)
  <> charUtf8 '\n'
  <> foldl' (\s f -> s <> charUtf8 '\n' <> fromLispExpr (List [Symbol "assert", toLisp f])) mempty (smt^.formulas)
  <> charUtf8 '\n'

buildVars :: Seq (SomeKnownSMTRepr SMTVar) -> Builder
buildVars = foldl'
  (\s (SomeKnownSMTRepr v) ->
       s
    <> charUtf8 '\n'
    <> fromLispExpr (List [Symbol "declare-fun", toLisp v, Symbol "()", goSing v]))
  mempty
    where
      goSing :: forall t. KnownSMTRepr t => SMTVar t -> Lisp
      goSing _ = toLisp $ singRepr @t
