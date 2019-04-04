# EEC 0.2.1-RC1

## Requirements
* Dotty 0.13.0-RC1 <http://dotty.epfl.ch>

## Usage

* Load sbt with `sbt`
* To run tests, in the sbt CLI use `test`.
* To launch the EEC REPL, in the sbt CLI use `run -e`.

## REPL
* `:help` to get a list of commands available.
* `:tf` to type check a file in the current directory, e.g. `Isomorphisms.hs`; adding any definitions to the environment.
* `:ctx` to print the current environment after defining terms.
* `:reset` to clear all definitions in the environment.
* `:def` to define a new top level definition.
* `:t` to type an expression.
* `:ast` to print the AST of an expression.
* `:astt` to print the AST of a top level definition.
* `:astf` to print the AST of a source file in the current directory.
* `:q` to quit the REPL.