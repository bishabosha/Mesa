# Changelog
All notable changes to this project will be documented in this file.

## [Unreleased]

## [0.2.1-RC1] - 2019-04-4
### Added
- Separation of scopes for data definitions and normal terms, leading to more
  flexibility in defining terms. Also prevents shadowing of imported data types,
  such as the bootstrapped types in the `_root_` package.

## [0.2.0-RC1] - 2019-04-4
### Added
- Type checking that ensures all possible cases in a match expression or
  linear match expression are provided.
- `data` definitions have been added that allow the user to define non-recursive
  sum types that may be used in match expressions and linear match expressions.
- `DataTypes.hs` demo file added.

### Changed
- Updated `Isomorphisms.hs` with new data definitions, replacing `Either A B`
  for `L |: R`, a new data type, and providing an implementation of `+:`.

### Removed
- `Either` and `+:` have been removed from the compiler's bootstrapped types.
  The user may now define these data types themselves and enjoy the same
  semantics.

## [0.1.2-RC1] - 2019-04-1
### Added
- Stricter semantics for type checking of linear lambdas, variables,
  constants and `!` terms in linear contexts.
- Addition of `?` a.k.a "why not", to summon `A` from `Void` in linear contexts.

### Changed
- Updated links on [README.md](README.md)
- Now using minor version numbering.
- Update [eec/Isomorphisms.hs](eec/Isomorphisms.hs) after addition of `?`.

## [0.1-RC1] - 2019-03-31
### Added
* Read Eval Print Loop
  - Define `EEC+` terms and typecheck them.
  - Print the parsed AST for `EEC+` terms and source files.
  - Print the derived types of `EEC+` terms and source files.
  - Print the current environment of terms and types.

* Linear types that enforce the *linear usage of effects*.
  - Proof of isomorphisms in Proposition 4.1. of Egger, Ejlers and Simpson (2014) can be found in [eec/Isomorphisms.hs](eec/Isomorphisms.hs).

* Haskell-like syntax
  - Source files use `.hs` suffix at present to benefit from syntax highlighting.
  - Refer to [eec/src/main/antlr4/EEC.g4](eec/src/main/antlr4/EEC.g4) for a context free grammar.

[Unreleased]: https://github.com/bishabosha/EEC/compare/0.2.1-RC1...HEAD
[0.2.1-RC1]: https://github.com/bishabosha/EEC/releases/tag/0.2.1-RC1
[0.2.0-RC1]: https://github.com/bishabosha/EEC/releases/tag/0.2.0-RC1
[0.1.2-RC1]: https://github.com/bishabosha/EEC/releases/tag/0.1.2-RC1
[0.1-RC1]: https://github.com/bishabosha/EEC/releases/tag/0.1-RC1