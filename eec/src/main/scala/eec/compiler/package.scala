package eec
package compiler

val preludeDefs = List(
  "primitive Left   l   : L -> Either L R",
  "primitive Right  r   : R -> Either L R",
  "primitive absurd [v] : Void# |- A#",
  "primitive InL    [l] : L# |- L# +: R#",
  "primitive InR    [r] : R# |- L# +: R#",
)