{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LiberalTypeSynonyms #-}

module Language.Hasmtlib.Internal.Parser where

import Prelude hiding (not, (&&), (||), and , or)
import Language.Hasmtlib.Internal.Render
import Language.Hasmtlib.Boolean
import Language.Hasmtlib.Codec
import Language.Hasmtlib.Type.Value
import Language.Hasmtlib.Type.Bitvec
import Language.Hasmtlib.Type.SMTSort
import Language.Hasmtlib.Type.Solution
import Language.Hasmtlib.Type.ArrayMap
import Language.Hasmtlib.Type.Expr
import Data.Bit
import Data.Coerce
import Data.Proxy
import Data.Ratio ((%))
import Data.ByteString hiding (filter, foldl)
import Data.ByteString.Builder
import Data.Attoparsec.ByteString hiding (Result, skipWhile, takeTill)
import Data.Attoparsec.ByteString.Char8 hiding (Result)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text as Text
import Control.Applicative
import Control.Lens hiding (op)
import GHC.TypeNats

answerParser :: Parser (Result, Solution)
answerParser = do
  result  <- resultParser
  model   <- anyModelParser

  return (result, model)

resultParser :: Parser Result
resultParser = (string "sat" *> pure Sat)
           <|> (string "unsat" *> pure Unsat)
           <|> (string "unknown" *> pure Unknown)

anyModelParser :: Parser Solution
anyModelParser = smt2ModelParser <|> defaultModelParser <|> return mempty

-- For the format CVC5 and Z3 use - what is it called?
defaultModelParser :: Parser Solution
defaultModelParser = do
  _       <- skipSpace >> char '(' >> skipSpace
  varSols <- many $ parseSomeSol <* skipSpace
  _       <- (skipSpace >> char ')' >> skipSpace) <|> skipSpace

  return $ fromSomeVarSols varSols

smt2ModelParser :: Parser Solution
smt2ModelParser = do
  _       <- skipSpace >> char '(' >> skipSpace >> string "model" >> skipSpace
  varSols <- many $ parseSomeSol <* skipSpace
  _       <- (skipSpace >> char ')' >> skipSpace) <|> skipSpace

  return $ fromSomeVarSols varSols

parseSomeSol :: Parser (SomeKnownOrdSMTSort SMTVarSol)
parseSomeSol = do
  _     <- char '(' >> skipSpace
  _     <- string "define-fun" >> skipSpace
  _     <- string "var_"
  vId   <- decimal @Int
  _     <- skipSpace >> string "()" >> skipSpace
  (SomeSMTSort someSort) <- parseSomeSort
  _     <- skipSpace
  expr  <- parseExpr' someSort
  _     <- skipSpace >> char ')'
  case decode mempty expr of
    Nothing    -> fail $ "Solver reponded with solution for var_" ++ show vId ++ " but it contains "
                      ++ "another var. This cannot be parsed and evaluated currently."
    Just value -> return $ SomeSMTSort $ SMTVarSol (coerce vId) (wrapValue value)
{-# INLINEABLE parseSomeSol #-}

parseSomeSort :: Parser (SomeKnownOrdSMTSort SSMTSort)
parseSomeSort = (string "Bool" *> pure (SomeSMTSort SBoolSort))
        <|> (string "Int"  *> pure (SomeSMTSort SIntSort))
        <|> (string "Real" *> pure (SomeSMTSort SRealSort))
        <|> parseSomeBitVecSort
        <|> parseSomeArraySort
        <|> (string "String" *> pure (SomeSMTSort SStringSort))
{-# INLINEABLE parseSomeSort #-}

parseSomeBitVecSort :: Parser (SomeKnownOrdSMTSort SSMTSort)
parseSomeBitVecSort = do
  _ <- char '(' >> skipSpace >> char '_' >> skipSpace
  _ <- string "BitVec" >> skipSpace
  n <- decimal
  _ <- skipSpace >> char ')'
  case someNatVal $ fromInteger n of
    -- SMTLib does not differentiate between signed and unsigned BitVec on the type-level
    -- We do. So we always just put Unsigned here and in Codec (Expr t)
    -- if (t ~ BvSort Signed _) we retrieve unsigned solution and flip type-level encoding
    SomeNat pn -> return $ SomeSMTSort $ SBvSort (Proxy @Unsigned) pn
{-# INLINEABLE parseSomeBitVecSort #-}

parseSomeArraySort :: Parser (SomeKnownOrdSMTSort SSMTSort)
parseSomeArraySort = do
  _ <- char '(' >> skipSpace
  _ <- string "Array" >> skipSpace
  (SomeSMTSort keySort)   <- parseSomeSort
  _ <- skipSpace
  (SomeSMTSort valueSort) <- parseSomeSort
  _ <- skipSpace >> char ')'
  return $ SomeSMTSort $ SArraySort (goProxy keySort) (goProxy valueSort)
    where
      goProxy :: forall t. SSMTSort t -> Proxy t
      goProxy _ = Proxy @t
{-# INLINEABLE parseSomeArraySort #-}

parseExpr' :: forall prxy t. KnownSMTSort t => prxy t -> Parser (Expr t)
parseExpr' _ = parseExpr @t
{-# INLINE parseExpr' #-}

-- TODO: Add parseSelect
parseExpr :: forall t. KnownSMTSort t => Parser (Expr t)
parseExpr = var <|> constantExpr <|> ternary "ite" (ite @(Expr BoolSort))
        <|> case sortSing @t of
              SIntSort  -> unary "abs" abs <|> unary  "-" negate
                      <|> nary "+" sum  <|> binary "-" (-) <|> nary "*" product <|> binary "mod" Mod
                      <|> unary "to_int" toIntSort <|> unary "str.len" strLength
                      <|> ternary "str.indexof" strIndexOf
              SRealSort -> unary "abs" abs <|> unary  "-" negate
                      <|> nary "+" sum  <|> binary "-" (-) <|> nary "*" product <|> binary "/" (/)
                      <|> unary "to_real" toRealSort
                      <|> smtPi <|> unary "sqrt" sqrt <|> unary "exp" exp
                      <|> unary "sin" sin <|> unary "cos" cos <|> unary "tan" tan
                      <|> unary "arcsin" asin <|> unary "arccos" acos <|> unary "arctan" atan
              SBoolSort -> unary "not" not
                      <|> nary "and" and  <|> nary "or" or <|> binary "=>" (==>) <|> binary "xor" xor
                      <|> binary @IntSort  "=" (===) <|> binary @IntSort  "distinct" (/==)
                      <|> binary @RealSort "=" (===) <|> binary @RealSort "distinct" (/==)
                      <|> binary @BoolSort "=" (===) <|> binary @BoolSort "distinct" (/==)
                      <|> binary @IntSort "<" (<?) <|> binary @IntSort "<=" (<=?)
                      <|> binary @IntSort ">=" (>=?) <|> binary @IntSort ">" (>?)
                      <|> binary @RealSort "<" (<?) <|> binary @RealSort "<=" (<=?)
                      <|> binary @RealSort ">=" (>=?) <|> binary @RealSort ">" (>?)
                      <|> binary @StringSort "str.<" (<?) <|> binary @StringSort "str.<=" (<=?)
                      <|> unary "is_int" isIntSort
                      <|> binary "str.prefixof" strPrefixOf <|> binary "str.suffixof" strSuffixOf
                      <|> binary "str.contains" strContains
                      -- TODO: Add compare ops for all (?) bv-sorts
              SBvSort enc _ -> unary "bvnot" not
                      <|> binary "bvand" (&&)  <|> binary "bvor" (||) <|> binary "bvxor" xor <|> binary "bvnand" BvNand <|> binary "bvnor" BvNor
                      <|> unary  "bvneg" negate
                      <|> binary "bvadd" (+)  <|> binary "bvsub" (-) <|> binary "bvmul" (*)
                      <|> binary "bvudiv" div <|> binary "bvurem" rem
                      <|> binary "bvshl" BvShL
                      <|> case bvEncSing' enc of SUnsigned -> binary "bvlshr" BvLShR ; SSigned -> binary "bvashr" BvAShR
              SArraySort _ _ -> ternary "store" ArrStore
                      -- TODO: Add compare ops for all (?) array-sorts
              SStringSort -> binary "str.++" (<>) <|> binary "str.at" strAt <|> ternary "str.substr" StrSubstring
                      <|> ternary "str.replace" strReplace <|> ternary "str.replace_all" strReplaceAll

var :: KnownSMTSort t => Parser (Expr t)
var = do
  _     <- string "var_"
  vId <- decimal @Int
  return $ Var $ coerce vId
{-# INLINE var #-}

constant :: forall t. KnownSMTSort t => Parser (HaskellType t)
constant = case sortSing @t of
  SIntSort       -> anyValue decimal
  SRealSort      -> anyValue parseRatioDouble <|> parseToRealDouble <|> anyValue rational
  SBoolSort      -> parseBool
  SBvSort _ p    -> anyBitvector p
  SArraySort k v -> constArray k v
  SStringSort    -> parseSmtString
{-# INLINEABLE constant #-}

constantExpr :: forall t. KnownSMTSort t => Parser (Expr t)
constantExpr = Constant . wrapValue <$> constant @t
{-# INLINE constantExpr #-}

anyBitvector :: (KnownBvEnc enc, KnownNat n) => Proxy n -> Parser (Bitvec enc n)
anyBitvector p = binBitvector p <|> hexBitvector p <|> literalBitvector p
{-# INLINE anyBitvector #-}

binBitvector :: KnownNat n => Proxy n -> Parser (Bitvec enc n)
binBitvector p = do
  _  <- string "#b" >> skipSpace
  bs <- many $ char '0' <|> char '1'
  let bs' :: [Bit] = fmap (\b -> ite (b == '1') true false) bs
  case bitvecFromListN' p bs' of
    Nothing -> fail $ "Expected BitVector of length" <> show (natVal p) <> ", but got a different one"
    Just v  -> return v
{-# INLINEABLE binBitvector #-}

hexBitvector :: (KnownBvEnc enc, KnownNat n) => Proxy n -> Parser (Bitvec enc n)
hexBitvector _ = do
  _ <- string "#x" >> skipSpace
  fromInteger <$> hexadecimal
{-# INLINE hexBitvector #-}

literalBitvector :: (KnownBvEnc enc, KnownNat n) => Proxy n -> Parser (Bitvec enc n)
literalBitvector _ = do
  _ <- char '(' >> skipSpace
  _ <- char '_' >> skipSpace
  _ <- string "bv"
  x <- decimal
  _ <- skipWhile (/= ')') >> char ')'

  return $ fromInteger x
{-# INLINE literalBitvector #-}

constArray :: forall k v. (KnownSMTSort v, Ord (HaskellType k)) => Proxy k -> Proxy v -> Parser (ConstArray (HaskellType k) (HaskellType v))
constArray _ _ = do
  _ <- char '(' >> skipSpace >> char '(' >> skipSpace
  _ <- string "as" >> skipSpace >> string "const" >> skipSpace
  _ <- char '(' >> skipWhile (/= ')') >> char ')' >> skipSpace
  _ <- char ')' >> skipSpace
  constVal <- constant @v
  _ <- skipSpace >> char ')'

  return $ asConst constVal
{-# INLINEABLE constArray #-}

parseSmtString :: Parser Text.Text
parseSmtString = do
  _ <- char '"'
  s <- decodeUtf8 <$> takeTill (== '"')
  _ <- char '"'
  return s

unary :: forall t r. KnownSMTSort t => ByteString -> (Expr t -> Expr r) -> Parser (Expr r)
unary opStr op = do
  _ <- char '(' >> skipSpace
  _ <- string opStr >> skipSpace
  val <- parseExpr
  _ <- skipSpace >> char ')'

  return $ op val
{-# INLINE unary #-}

binary :: forall t u r. (KnownSMTSort t, KnownSMTSort u) => ByteString -> (Expr t -> Expr u -> Expr r) -> Parser (Expr r)
binary opStr op = do
  _ <- char '(' >> skipSpace
  _ <- string opStr >> skipSpace
  l <- parseExpr
  _ <- skipSpace
  r <- parseExpr
  _ <- skipSpace >> char ')'
  return $ l `op` r
{-# INLINE binary #-}

ternary :: forall t u v r. (KnownSMTSort t, KnownSMTSort u, KnownSMTSort v) => ByteString -> (Expr t -> Expr u -> Expr v -> Expr r) -> Parser (Expr r)
ternary opStr op = do
  _ <- char '(' >> skipSpace
  _ <- string opStr >> skipSpace
  l <- parseExpr
  _ <- skipSpace
  m <- parseExpr
  _ <- skipSpace
  r <- parseExpr
  _ <- skipSpace >> char ')'
  return $ op l m r
{-# INLINE ternary #-}

nary :: forall t r. KnownSMTSort t => ByteString -> ([Expr t] -> Expr r) -> Parser (Expr r)
nary opStr op = do
  _    <- char '(' >> skipSpace
  _    <- string opStr >> skipSpace
  args <- parseExpr `sepBy1` skipSpace
  _    <- skipSpace >> char ')'
  return $ op args
{-# INLINE nary #-}

smtPi :: Parser (Expr RealSort)
smtPi = string "real.pi" *> return pi
{-# INLINE smtPi #-}

anyValue :: Num a => Parser a -> Parser a
anyValue p = negativeValue p <|> p
{-# INLINEABLE anyValue #-}

negativeValue :: Num a => Parser a -> Parser a
negativeValue p = do
  _ <- char '(' >> skipSpace >> char '-' >> skipSpace
  val <- signed p
  _ <- skipSpace >> char ')'

  return $ negate val
{-# INLINE negativeValue #-}

parseRatioDouble :: Parser Double
parseRatioDouble = do
  _           <- char '(' >> skipSpace >> char '/' >> skipSpace
  numerator   <- decimal
  _           <- skipSpace
  denominator <- decimal
  _           <- skipSpace >> char ')'

  return $ fromRational $ numerator % denominator
{-# INLINEABLE parseRatioDouble #-}

parseToRealDouble :: Parser Double
parseToRealDouble = do
  _   <- char '(' >> skipSpace >> string "to_real" >> skipSpace
  dec <- anyValue decimal
  _   <- skipSpace >> char ')'

  return $ fromInteger dec
{-# INLINEABLE parseToRealDouble #-}

parseBool :: Parser Bool
parseBool = (string "true" *> pure True) <|> (string "false" *> pure False)
{-# INLINEABLE parseBool #-}

getValueParser :: KnownSMTSort t => SMTVar t -> Parser (SMTVarSol t)
getValueParser v = do
  _ <- char '(' >> skipSpace >> char '(' >> skipSpace
  _ <- string $ toStrict $ toLazyByteString $ render v
  _ <- skipSpace
  expr <- parseExpr
  _ <- skipSpace >> char ')' >> skipSpace >> char ')'
  case decode mempty expr of
    Nothing    -> fail $ "Solver reponded with solution for var_" ++ show (v^.varId) ++ " but it contains "
                      ++ "another var. This cannot be parsed and evaluated currently."
    Just value -> return $ SMTVarSol v (wrapValue value)
