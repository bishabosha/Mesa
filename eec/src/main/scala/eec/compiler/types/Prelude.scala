package eec.compiler.types

import eec.compiler.core.Contexts.{Context, IdGen}
import eec.compiler.error.CompilerErrors.{CompilerErrorOps,Lifted}
import eec.compiler.parsing.EntryPoint.parseDef
import Namers.indexed
import Typers.typed
import CompilerErrorOps._

object Prelude {
  private val prelude = """
  primitive absurd [v] : Void# ->. A#

  data L |: R = Left L | Right R
  data L# +: R# = InL[L#] | InR[R#]
  data List a = Cons a (List a) | Nil

  fstL [p] : (A#, B#) ->. A# = case p of (a, _) =>. a
  sndL [p] : (A#, B#) ->. B# = case p of (_, b) =>. b
  fst p    : (A, B) -> A     = case p of (a, _) => a
  snd p    : (A, B) -> B     = case p of (_, b) => b

  id x   : a -> a    = x
  idL[x] : A# ->. A# = x

  dup x    : A -> (A,A)     = (x,x)
  dupL [x] : A# ->. (A#,A#) = (x,x)

  (<<<) g f a : (B -> C) -> (A -> B) -> A -> C = g (f a)

  uncurry f p : (a -> b -> c) -> (a,b) -> c = case p of (a,b) => f a b

  x ?= y: A -> A -> () = () -- type level assertion that x and y are the same type
  """
  val source = prelude.linesIterator.map(_.trim).filter(_.nonEmpty).toList

  def importInScope given Context, IdGen: Lifted[Unit] = {
    source.foldLeftE(()) { (_,s) =>
      for
        expr <- parseDef(s)
        _    <- indexed(expr)
        _    <- expr.typed
      yield ()
    }
  }
}