![Static Badge](https://img.shields.io/badge/Lang-GHC2021-blue) [![Haskell-CI](https://github.com/bruderj15/Hasmtlib/actions/workflows/haskell-ci.yml/badge.svg)](https://github.com/bruderj15/Hasmtlib/actions/workflows/haskell-ci.yml) [![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

# Hasmtlib

Hasmtlib is a library for generating SMTLib2-problems using a monad.
It takes care of encoding your problem, marshaling the data to an external solver and parsing and interpreting the result into Haskell types.
It is highly inspired by [ekmett/ersatz](https://github.com/ekmett/ersatz) which does the same for QSAT. Communication with external solvers is handled by [tweag/smtlib-backends](https://github.com/tweag/smtlib-backends).

Building expressions with type-level representations of the SMTLib2-Types guarantees type-safety when communicating with an external solver.

Although Hasmtlib does not yet make use of _observable_ sharing [(StableNames)](https://downloads.haskell.org/ghc/9.6.1/docs/libraries/base-4.18.0.0/System-Mem-StableName.html#:~:text=Stable%20Names,-data%20StableName%20a&text=An%20abstract%20name%20for%20an,makeStableName%20on%20the%20same%20object.) like Ersatz does, sharing in the API still allows for pure formula construction.

Therefore, this allows you to use the much richer subset of Haskell than a purely monadic meta-language would, which [hgoes/smtlib2](https://github.com/hgoes/smtlib2) is one of. This ultimately results in extremely compact code.

For instance, to define the addition of two `V3` containing a Real-SMT-Expression:
```haskell
v3Add :: V3 (Expr RealType) -> V3 (Expr RealType) -> V3 (Expr RealType)
v3Add = liftA2 (+)
```
Even better, the [Expr-GADT](https://github.com/bruderj15/Hasmtlib/blob/master/src/Language/Hasmtlib/Internal/Expr.hs) allows for a polymorph definition:
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
  res <- solveWith cvc5 $ do
    setLogic "QF_NRA"

    u :: V3 (Expr RealType) <- variable
    v <- variable

    assert $ dot u v === 5

    return (u,v)

  print res
```
May print: `(Sat,Just (V3 (-2.0) (-1.0) 0.0,V3 (-2.0) (-1.0) 0.0))`

## Features

### Supported
- [x] SMTLib2 types in the haskell type
- [x] Full support for SMTLib2 types Int, Real, Bool and unsigned BitVec
- [x] Type-level length-indexed Bitvectors for BitVec
- [x] Pure API with Expression-instances for Num, Floating, Bounded, ...
- [x] Solvers via external process: CVC5, Z3, Yices2-SMT & MathSAT
- [x] Add your own solvers via the [Solver type](https://github.com/bruderj15/Hasmtlib/blob/master/src/Language/Hasmtlib/Type/Solver.hs)
- [x] Incremental solving

### Coming
- [ ] Observable sharing
- [ ] Quantifiers `for_all` and `exists`

## Examples
There are some examples in [here](https://github.com/bruderj15/Hasmtlib/tree/master/src/Language/Hasmtlib/Example).

## Contact information
Contributions, critics and bug reports are welcome!

Please feel free to contact me through GitHub.
