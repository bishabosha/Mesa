package eec
package compiler
package types

import org.junit.Test
import org.junit.Assert._

class TopDefTest {

  import core.Contexts._
  import ast.Trees._
  import parsing._
  import error.CompilerErrors._
  import error.CompilerErrors.CompilerError._
  import Types._
  import types.Typers._
  import error.CompilerErrors._

  val any = types.Types.Type.WildcardType

  @Test def typecheckTrivial(): Unit = {
    passesTypeCheck(
      "unit: () = ()".typed   -> "()")
  }

  @Test def typecheckEithers(): Unit = {
    passesTypeCheck(
      "l: Either () r = Left ()".typed  -> "Either () r",
      "r: Either l () = Right ()".typed -> "Either l ()")
  }

  @Test def typecheckLiftBind(): Unit = {
    passesTypeCheck(
      """lift f: (a -> !b) -> !a -> !b =
          \c: !a => let !x = c in f x""".typed   -> "(a -> ! b) -> ! a -> ! b",
      """ma >>= f: !t -> (t -> !u) -> !u =
          (lift f) ma""".typed                   -> "! t -> (t -> ! u) -> ! u")
  }

  def (str: String) typedAs(as: Type): () => given Context => Checked[Tree] = {
    import Namers._
    import CompilerErrorOps._
    () => for {
      exp <- parseStat(str)
      _   <- indexAsExpr(exp)
      tpd <- exp.typedAsExpr(any)
    } yield tpd
  }

  def (str: String) typed: () => given Context => Checked[Tree] = str.typedAs(any)

  def passesTypeCheck(seq: (() => given Context => Checked[Tree], String)*): Unit = {

    import CompilerErrorOps._
    import Namers._
    
    val rootCtx = new RootContext()
    implied for Context = rootCtx

    for {
      _ <-  Context.enterBootstrapped
      _ <-  compiler.preludeDefs.mapE { s =>
              for {
                exp <- parseStat(s)
                _   <- indexAsExpr(exp)
                tpd <- exp.typedAsExpr(any)
              } yield ()
            }
      _ <-  seq.toList.mapE((f, s) => impl(f(), s))
    } yield ()

    def impl(parsed: Checked[Tree], checkTpe: String): Unit = {
      import CompilerErrorOps._
      import implied CompilerErrorOps._
      import implied TypeOps._
      parsed.fold
        { err => fail(err.userString) }
        { tpd => assertEquals(checkTpe, tpd.tpe.userString) }
    }
  }

  def failsTypeCheck(seq: Checked[Tree]*): Unit = {
    def impl(parsed: Checked[Tree]): Unit = {
      import CompilerErrorOps._
      import implied TreeOps._
      import implied TypeOps._
      parsed.fold
        { err => () }
        { tpd => fail(
          s"Typed successfully as ${tpd.tpe.userString} for expr:\n${tpd.userString}")
        }
    }
    seq.foreach { impl }
  }
}