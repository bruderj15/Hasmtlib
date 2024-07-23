# Changelog

All notable changes to the hasmtlib library will be documented in this
file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [PVP versioning](https://pvp.haskell.org/).

## v2.0.0 _(2024-07-23)_

### Added
- Arithmetic functions: `isIntSort`, `toIntSort` & `toRealSort`
- Asserting maybe-formulas: `assertMaybe :: MonadSMT s m => Maybe (Expr BoolSort) -> m ()`
- Logical equivalence `(<==>)` & reverse logical implication
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
