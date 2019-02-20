package eec
package compiler

val preludeSource ="""
package eec.Prelude

primitive fst p: (a, b) -> a
primitive snd p: (a, b) -> b
primitive debug x: a -> String

Unit: () = ()
""".trim