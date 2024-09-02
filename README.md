[![Hackage](https://img.shields.io/hackage/v/hasmtlib.svg)](https://hackage.haskell.org/package/hasmtlib) ![Static Badge](https://img.shields.io/badge/Lang-GHC2021-blue) [![Haskell-CI](https://github.com/bruderj15/Hasmtlib/actions/workflows/haskell-ci.yml/badge.svg)](https://github.com/bruderj15/Hasmtlib/actions/workflows/haskell-ci.yml) [![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

# Hasmtlib - Haskell SMTLib MTL Library

Hasmtlib is a library with high-level-abstraction for generating SMTLib2-problems using a monad.
It takes care of encoding your problem, marshaling the data to an external solver and parsing and interpreting the result into Haskell types.
It is highly inspired by [ekmett/ersatz](https://github.com/ekmett/ersatz) which does the same for QSAT. Communication with external solvers is handled by [tweag/smtlib-backends](https://github.com/tweag/smtlib-backends).

Building expressions with **type-level representations** of the **SMTLib2-Sorts** guarantees type-safety when communicating with external solvers.

While **formula construction** is entirely **pure**, Hasmtlib - just like `ersatz` - makes use of _**observable sharing**_ for expressions.

This allows you to use the much richer subset of Haskell than a purely monadic meta-language would, which ultimately results in extremely compact code.

For instance, to define the addition of two `V3` containing Real-SMT-Expressions:
```haskell
v3Add :: V3 (Expr RealSort) -> V3 (Expr RealSort) -> V3 (Expr RealSort)
v3Add = liftA2 (+)
```
Even better, the [Expr-GADT](https://github.com/bruderj15/Hasmtlib/blob/master/src/Language/Hasmtlib/Type/Expr.hs) allows a polymorph definition:
```haskell
v3Add :: Num (Expr t) => V3 (Expr t) -> V3 (Expr t) -> V3 (Expr t)
v3Add = liftA2 (+)
```
This looks a lot like the [definition of Num](https://hackage.haskell.org/package/linear-1.23/docs/src/Linear.V3.html#local-6989586621679182277) for `V3 a`:
```haskell
instance Num a => Num (V3 a) where
  (+) :: V3 a -> V3 a -> V3 a
  (+) = liftA2 (+)
```
Hence, no extra definition is needed at all. We can use the existing instances:
```haskell
{-# LANGUAGE DataKinds #-}

import Language.Hasmtlib
import Linear

-- instances with default impl
instance Codec a => Codec (V3 a)
instance Variable a => Variable (V3 a)

main :: IO ()
main = do
  res <- solveWith @SMT (solver cvc5) $ do
    setLogic "QF_NRA"

    u :: V3 (Expr RealSort) <- variable
    v <- variable

    assert $ dot u v === 5

    return (u,v)

  print res
```
May print: `(Sat,Just (V3 (-2.0) (-1.0) 0.0,V3 (-2.0) (-1.0) 0.0))`

## Features

- [x] SMTLib2-Sorts in the Haskell-Type to guarantee well-typed expressions
  ```haskell
    data SMTSort =
        BoolSort
      | IntSort
      | RealSort
      | BvSort BvEnc Nat
      | ArraySort SMTSort SMTSort
      | StringSort
    data Expr (t :: SMTSort) where ...

    ite :: Expr BoolSort -> Expr t -> Expr t -> Expr t
  ```
- [x] Full SMTLib 2.6 standard support for Sorts Bool, Int, Real, BitVec, Array & String
- [x] Type-level length-indexed Bitvectors with type-level encoding (Signed/Unsigned) for BitVec
- [x] Pure API with plenty common instances: `Num`, `Floating`, `Bounded`, `Bits`, `Ixed` and many more
- [x] Add your own solvers via the [Solver type](https://github.com/bruderj15/Hasmtlib/blob/master/src/Language/Hasmtlib/Type/Solution.hs)
- [x] Solvers via external processes: CVC5, Z3, Yices2-SMT, MathSAT, OptiMathSAT, OpenSMT & Bitwuzla
- [x] Support for incremental solving
- [x] Pure quantifiers `for_all` and `exists`
  ```haskell
    solveWith @SMT (solver z3) $ do
      setLogic "LIA"
      z <- var @IntSort
      assert $ z === 0
      assert $
        for_all $ \x ->
            exists $ \y ->
              x + y === z
      return z
  ```
- [x] Optimization Modulo Theories (OMT) / MaxSMT
  ```haskell
    res <- solveWith @OMT (solver z3) $ do
      setLogic "QF_LIA"
      x <- var @IntSort

      assert $ x >? -2
      assertSoftWeighted (x >? -1) 5.0
      minimize x

      return x
  ```

## Related work
- [ekmett/ersatz](https://hackage.haskell.org/package/ersatz):
Huge inspiration for this library (some code stolen).
We do for `SMT` what they do for `SAT`.
- [hgoes/smtlib2](https://hackage.haskell.org/package/smtlib2):
Their eDSL is highly expressive and focuses on well-typed SMT-expressions.
But their approach is a little verbose and makes usage feel quite heavy.
Their eDSL is also entirely monadic and therefore formula construction is painful.
- [yav/simple-smt](https://hackage.haskell.org/package/simple-smt):
They are lightweight but their types are weak and their API is barely embedded into Haskell.
- [LevantErkok/sbv](https://hackage.haskell.org/package/sbv):
While they "express properties about Haskell programs and automatically prove them using SMT",
we instead use Haskell to simplify the encoding of SMT-Problems.
They can do a whole lot (C-Code-Gen, Crypto-Stuff,...), which is cool, but adds weight.

If you want highly specific implementations for different solvers, all their individual configurations and swallow the awkward typing,
then use [hgoes/smtlib2](https://hackage.haskell.org/package/smtlib2).

If you want to express properties about Haskell programs and automatically prove them using SMT,
then use [LevantErkok/sbv](https://hackage.haskell.org/package/sbv).

If you want to encode SMT-problems as lightweight and close to Haskell as possible, then use this library.
I personally use it for scheduling/resource-allocation-problems.

## Examples
There are some examples in [here](https://github.com/bruderj15/Hasmtlib/tree/master/src/Language/Hasmtlib/Example).

## Contact information
Contributions, critics and bug reports are welcome!

Please feel free to contact me through GitHub.
