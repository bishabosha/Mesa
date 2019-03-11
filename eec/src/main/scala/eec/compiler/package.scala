package eec
package compiler

val preludeDefs = List(
  "primitive Left x: l -> Either l r",
  "primitive Right x: r -> Either l r"
)