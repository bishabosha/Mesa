package eec
package compiler

val preludeDefs = List(
  """ data L |: R =
        Left L
      | Right R """,

  """ data L# +: R# =
        InL[L#]
      | InR[R#] """,

  """ primitive absurd [v] : Void# -○ A# """,
)