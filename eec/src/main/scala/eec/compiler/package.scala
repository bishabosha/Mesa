package eec
package compiler

val preludeDefs = List(
  """ data Either L R =
        Left L
      | Right R """,
  "primitive absurd [v] : Void# |- A#",
  "primitive InL    [l] : L# |- L# +: R#",
  "primitive InR    [r] : R# |- L# +: R#",
)