# EEC

### Current Release
0.1.2-RC1

## About
This repository is for implementing a programming language based on the enriched effect calculus by Egger, Ejlers and Simpson (2014).

## Features
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

## Author
* James Thompson, BSc. University of Bath.

## Usage
* Refer to [eec/README.md](eec/README.md)

## References
- *Egger, J., Ejlers, R. and Simpson, A., 2014. The enriched effect calculus: syntax and semantics. Journal of Logic and Computation, 24(3), pp.615-654.*
