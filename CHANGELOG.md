# Changelog

All notable changes to the hasmtlib library will be documented in this
file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [PVP versioning](https://pvp.haskell.org/).

## v2.6.3 _(2024-09-07)_

### Added
- Added a solver configuration `bitwuzlaKissat` for `Bitwuzla` with underlying SAT-Solver `Kissat`.

### Changed
- Removed `solveMinimizedDebug` & `solveMaximizedDebug`. Use the modified `solveMinimized` & `solveMaximized` instead.
You can also provide a step-size now.
- Fixed a bug where `MonadOMT#solve` would run `get-model` although the solver did not necessarily respond with `Sat`.
- `SharingMode` for sharing common (sub-)expressions now defaults to `None`.
The previous default `StableNames` in general is only worth using, when your program can benefit a lot from sharing.
Otherwise it may drastically downgrade solver performance due to abundance of sharing-variables.
If you still want to use it, run `setSharingMode StableNames` within the problems monad.

## v2.6.2 _(2024-09-04)_

### Changed
- Functions for enconding cardinality-constraints in `Language.Hasmtlib.Counting` now use specialized encodings for some cardinalities - improving solvers efficiency.

## v2.6.1 _(2024-09-02)_

### Added
- Added module 'Language.Hasmtlib.Type.Relation' for encoding binary relations
- Added rich documentation and usage examples for all public modules

## v2.6.0 _(2024-08-27)_

### Added
- Support for signed BitVec operations.
- Added constructor `Rem` for `Expr t`.

### Changed
- *(breaking change)* Enhanced the type of `BvSort Nat` to `BvSort BvEnc Nat` where `BvEnc = Unsigned | Signed`.
  Before, the API only allowed unsigned BitVec, therefore `BvSort n` now becomes `BvSort Unsigned n`.
  The promoted type `BvEnc` is phantom and only used for differentiating instances for `Num`, ...
- Moved `Language.Hasmtlib.Internal.Bitvec` to `Language.Hasmtlib.Type.Bitvec`, exported API with `Language.Hasmtlib`
- Removed constructors `StrLT` and `StrLTHE` from `Expr t`.
- Fixed wrong implementation of Num for `Bitvec`. `(+)`, `(-)` and `(*)` had invalid definitions.

## v2.5.1 _(2024-08-26)_

### Added
- Added `SharingMode = None | StableNames` in `Language.Hasmtlib.Internal.Sharing`. Defaults to `StableNames`.
- Added function `setSharingMode` which allows you to change the `SharingMode`.

### Changed
- `runSharing` in `Language.Hasmtlib.Internal.Sharing` now differentiates sharing behavior based on newly given argument of type `SharingMode`

## v2.5.0 _(2024-08-25)_

### Added
- added instances `Eq`, `Ord`, `GEq` and `GCompare` for `Expr t`
- added instances `Real` and `Enum` for `Expr IntSort`, `Expr RealSort` and `Expr (BvSort n)`
- added instance `Integral` for `Expr IntSort` and `Expr (BvSort n)`
- added instance `Bits` for `Expr BoolSort` and `Expr (BvSort n)`

### Changed
- Removed `Language.Hasmtlib.Integraled`: use the added `Integral` instance instead
- Removed redundant BitVec constructors from `Expr` and replaced usage in instances with the more generic existing ones.
For example: Where `BvNot` was used previously, we now use `Not` which is already used for Expr BoolSort.
Differentiation between such operations now takes place in `Language.Hasmtlib.Internal.Render#render` when rendering expressions,
e.g. rendering `bvnot` for `BvSort` and `not` for `BoolSort`.
Therefore there is no behavioral change for the solver.
- Removed functions `bvRotL` and `bvRotR` from `Language.Hasmtlib.Type.Expr`: use the added `Bits` instance instead with `rotateL` and `rotateR`

## v2.4.0 _(2024-08-21)_

### Added
- Added _**observable** sharing_ with `Language.Hasmtlib.Internal.Sharing`. Thank you fabeulous@github for the great help!
- Added `Language.Hasmtlib.Internal.Uniplate1` for plating GADTs

### Changed
- Deleted and moved `Language.Hasmtlib.Equatable`, `Language.Hasmtlib.Orderable`, `Language.Hasmtlib.Iteable` & `Language.Hasmtlib.Internal.Expr` into `Language.Hasmtlib.Type.Expr`

## v2.3.2 _(2024-08-17)_

### Changed
- Internal replacement of `Prelude.liftA2` for backwards compatiblity
- Fixed bug where `solveMaximizedDebug` would solve minimized instead of maximized

## v2.3.1 _(2024-08-16)_

### Changed
- Instances for `Boolean`, `Num` & `Fractional` on `Expr` now use smart constructors

## v2.3.0 _(2024-08-12)_

### Added
- Added full SMTLib2.6-standard support for sort String
- Added module `Language.Hasmtlib.Lens` featuring `instance Plated (Expr t)` for rewriting

### Changed
- Export constructors of `Expr t`
- `instance Show (Expr t)` now displays expressions in SMTLib2-Syntax

## v2.2.0 _(2024-08-09)_

### Added
- Added multiple basic instances for `Equatable`, `Orderable`, `Iteable`, `Variable` & `Codec`

### Changed
- *(breaking change)* `Variable` & `Codec` now ship with a `Generic` default implementation instead of relying on `Applicative`. This will break your instances if the type does not have an instance for `Generic`.

## v2.1.0 _(2024-07-26)_

### Added
- Added solver Bitwuzla
- Added debugging capabilities for `Pipe` by introducing `debugInteractiveWith`

### Changed
- Yices now uses flag `--incremental` by default
- *(breaking change)* Removed functional dependency `m -> s` from `MonadIncrSMT s m`. This forces you to specify the underlying state when using `interactiveWith`. Therefore `interactiveWith cvc5Living $ do ...` now becomes `interactiveWith @Pipe cvc5Living $ do ...`.
- *(breaking change)* Removed `newtype ProcessSolver` and replaced it with underlying `SMTLIB.Backends.Process.Config`. This may only affect you if you instantiated custom solvers.

## v2.0.1 _(2024-07-23)_

### Added
- Added more documentation

### Changed
- `(<==>)` now has `infixr 4`
- `(<==)` now has `infixl 0`

## v2.0.0 _(2024-07-23)_

### Added
- Arithmetic functions: `isIntSort`, `toIntSort` & `toRealSort`
- Asserting maybe-formulas: `assertMaybe :: MonadSMT s m => Maybe (Expr BoolSort) -> m ()`
- Logical equivalence `(<==>)` & reverse logical implication `(<==)`
- Solvers: OpenSMT, OptiMathSAT
- Iterative refinement optimizations utilizing incremental stack
- Custom solver options via `Custom String String :: SMTOption`
- Optimization Modulo Theories (OMT) / MaxSMT support with: `minimize`, `maximize` & `assertSoft`

### Changed
- *(breaking change)* The functions `solver` and `debug` which create `Solver`s from `ProcessSolver`s are polymorph in the state `s` now.
This requires you to annotate the mentioned functions with the targetted state.
These are `SMT` and `OMT`.
For example, `solverWith (solver cvc5) $ do ...` becomes `solverWith (solver @SMT cvc5) $ do ...`.
- Prefix `xor` now has an `infix 4`

## v1.3.1 _(2024-07-12)_

### Added
- Added `count` in `Language.Hasmtlib.Counting`

## v1.3.0 _(2024-07-12)_

### Added
- Added cardinality constraints with `Language.Hasmtlib.Counting`

## v1.2.0 _(2024-07-11)_

### Added
- Added n-ary comparisons `distinct` & `equal`

### Changed
- *(breaking change)* When using `interactiveWith` the `SMTOption` `Incremental` is no longer set by default anymore

## v1.1.2 _(2024-07-02)_

### Changed
- Minor internal changes

## v1.1.1 _(2024-06-25)_

### Added
- Added this `CHANGELOG.md` file

## v1.1.0 _(2024-06-21)_

### Added
- Added `ArraySort` and full support for its SMTLib2 standard specification
