package eec
package compiler

object Prelude {
  val preludeDefs = List(
    """ data L |: R =
          Left L
        | Right R """,

    """ data L# +: R# =
          InL[L#]
        | InR[R#] """,

    """ primitive absurd [v] : Void# ->. A# """,
  )
}