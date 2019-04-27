package eec
package compiler

object Prelude {
  private val prelude = """
  primitive absurd [v] : Void# ->. A#

  data L |: R = Left L | Right R
  data L# +: R# = InL[L#] | InR[R#]

  fstL [p] : (A#, B#) ->. A# = case p of (a, _) =>. a
  sndL [p] : (A#, B#) ->. B# = case p of (_, b) =>. b
  fst p    : (A, B) -> A     = case p of (a, _) => a
  snd p    : (A, B) -> B     = case p of (_, b) => b

  id x: a -> a = x
  idL[x] : A# ->. A# = x

  (<<<) g f a : (B -> C) -> (A -> B) -> A -> C = g (f a)

  uncurry f p : (a -> b -> c) -> (a,b) -> c = case p of (a,b) => f a b

  x ?= y: A -> A -> () = () -- type level assertion that x and y are the same type
  """
  val preludeDefs = prelude.linesIterator.map(_.trim).filter(_.nonEmpty).toList
}