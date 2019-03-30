package eec
package compiler

val preludeDefs = List(
  "primitive absurd v   : Void -> a",
  "primitive Left   l   : L -> Either L R",
  "primitive Right  r   : R -> Either L R",
  "primitive InL    [l] : L# -* L# |+| R#",
  "primitive InR    [r] : R# -* L# |+| R#"
)