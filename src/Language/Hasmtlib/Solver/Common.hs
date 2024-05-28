module Language.Hasmtlib.Solver.Common where

import Language.Hasmtlib.Type.SMT
import Language.Hasmtlib.Type.Expr
import Data.AttoLisp
import Data.ByteString.Builder
import Data.Sequence hiding ((|>), filter)
import Control.Lens hiding (List)
import qualified Data.Text as T
  
buildSMT :: SMT -> Seq Builder
buildSMT smt =
     fromList (fromLispExpr . toLisp <$> smt^.options)
  >< maybe mempty (\l -> singleton $ fromLispExpr (List [Symbol "set-logic", Symbol (T.pack l)])) (smt^.mlogic)
  >< buildVars (smt^.vars)
  >< fmap (\f -> fromLispExpr (List [Symbol "assert", toLisp f])) (smt^.formulas)
  
buildVars :: Seq (SomeKnownSMTRepr SMTVar) -> Seq Builder
buildVars = fmap (\(SomeKnownSMTRepr v) -> fromLispExpr (List [Symbol "declare-fun", toLisp v, Symbol "()", goSing v]))
    where
      goSing :: forall t. KnownSMTRepr t => SMTVar t -> Lisp
      goSing _ = toLisp $ singRepr @t
