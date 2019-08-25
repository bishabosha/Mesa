# Mesa

## Current Release
0.3.4-SNAPSHOT

## About
This repository is for implementing a programming language based on the enriched effect calculus by Egger, Ejlers and Simpson (2014).

## Changes
* Refer to [CHANGELOG.md](CHANGELOG.md)

## Features
* Read Eval Print Loop
  - Define Mesa terms and type-check them.
  - Print the meta AST for Mesa terms and source files.
  - Print the derived types of Mesa terms and source files.
  - Print the current environment of terms and types.

* Linear types that enforce the *linear usage of effects*.
  - Proof of isomorphisms in Proposition 4.1. of Egger, Ejlers and Simpson (2014) can be found in [eec/Isomorphisms.hs](eec/Isomorphisms.hs).

* Haskell-like syntax
  - Source files use `.hs` suffix at present to benefit from syntax highlighting.
  - Refer to [eec/src/main/antlr4/EEC.g4](eec/src/main/antlr4/EEC.g4) for a context free grammar.

## Author
* James Thompson, BSc. University of Bath.

## Usage

* Load sbt with `sbt`
* To run tests, in the sbt CLI use `test`.
* To launch the EEC REPL, in the sbt CLI use `run -e`.

## REPL
* `:help` to get a list of commands available.
* `:tf` to type check a file in the current directory, e.g. `mesa/Isomorphisms.hs`; adding any definitions to the environment.
* `:ctx` to print the current environment after defining terms.
* `:reset` to clear all definitions in the environment.
* `:def` to define a new top level definition.
* `:t` to type an expression.
* `:ast` to print the AST of an expression.
* `:astt` to print the AST of a top level definition.
* `:astf` to print the AST of a source file in the current directory.
* `:q` to quit the REPL.

## References
- *Egger, J., Ejlers, R. and Simpson, A., 2014. The enriched effect calculus: syntax and semantics. Journal of Logic and Computation, 24(3), pp.615-654.*
