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

  private val any = Types.Type.WildcardType

  private def typeStat(stat: String) given Context =
    for {
      exp <- parseStat(stat)
      _   <- indexAsExpr(exp)
      tpd <- exp.typedAsExpr(any)
    } yield ()

  def initialCtx: Checked[Context] = {

    val rootCtx = new RootContext()
    implied for Context = rootCtx

    for {
      _ <- Context.enterBootstrapped
      _ <- compiler.preludeDefs.mapE(typeStat)
    } yield rootCtx
  }
}