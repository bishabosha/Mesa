package eec
package compiler
package types

object BootstrapTests {

  import core.Contexts._
  import error.CompilerErrors._
  import parsing.EntryPoint._
  import Typers._
  import Namers._
  import CompilerErrorOps._
  import Namers._
  import Types._
  import ast.Trees._

  import org.junit.Test
  import org.junit.Assert._

  private val any = Type.WildcardType

  def typeExpr: String => Type => Contextual[IdMaker[Checked[Tree]]] =
    typeCore(parseExpr)

  def typeStat: String => Type => Contextual[IdMaker[Checked[Tree]]] =
    typeCore(parseStat)

  private def typeCore(f: String => IdMaker[Checked[Tree]])
                      (str: String)
                      (pt: Type) given IdGen, Context =
    for {
      exp <- f(str)
      _   <- indexAsExpr(exp)
      tpd <- exp.typedAsExpr(pt)
    } yield tpd

  def failIfTyped(tpd: Checked[Tree]): Unit = {
    import implied TreeOps._
    import implied TypeOps._
    tpd.fold
      { err => () }
      { tpd => fail(
        s"Typed successfully as ${tpd.tpe.userString} for expr:\n${tpd.userString}")
      }
  }

  def checkTpe(parsed: Checked[Tree], checkTpe: String): Unit = {
      import implied CompilerErrorOps._
      import implied TypeOps._
      parsed.fold
        { err => fail(err.userString) }
        { tpd => assertEquals(checkTpe, tpd.tpe.userString) }
    }

  def initialCtx: Checked[(IdGen, Context)] = {

    val idGen = new IdGen
    val ctx   = new RootContext()
    implied for Context = ctx
    implied for IdGen   = idGen

    for {
      _ <- Context.enterBootstrapped
      _ <- compiler.preludeDefs.mapE(typeStat(_)(any))
    } yield (idGen, ctx)
  }
}