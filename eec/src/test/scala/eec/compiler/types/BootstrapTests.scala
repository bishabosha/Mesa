package eec
package compiler
package types

import core.Contexts._
import core.Names._
import error.CompilerErrors._
import parsing.EntryPoint._
import Typers._
import Namers._
import CompilerErrorOps._
import Namers._
import Types._
import ast.Trees._
import util.Convert
import Convert._

import org.junit.Test
import org.junit.Assert._

import implied CompilerErrorOps._
import implied TreeOps._
import implied TypeOps._
import implied NameOps._

object BootstrapTests {

  private val any = Type.WildcardType

  def typeExpr: String => Type => Contextual[IdReader[Checked[Tree]]] =
    typeCore(parseExpr)

  def typeStat: String => Type => Contextual[IdReader[Checked[Tree]]] =
    typeCore(parseStat)

  private def typeCore(f: String => IdReader[Checked[Tree]])
                      (str: String)
                      (pt: Type) given IdGen, Context =
    for {
      exp <- f(str)
      _   <- indexAsExpr(exp)
      tpd <- exp.typedAsExpr(pt)
    } yield tpd

  def failIfTyped(tpd: Checked[Tree]): Unit = {
    tpd.fold
      { err => () }
      { tpd =>
        val tpeStr = tpd.tpe.show
        val tpdStr = tpd.show
        fail(s"Typed successfully as $tpeStr for expr:\n$tpdStr")
      }
  }

  def failIfAllTyped(tpd: Checked[Iterable[Tree]]): Unit = {
    tpd.fold
      { err => () }
      { tpds =>
        val tpes = tpds.map { t =>
          val nameStr = (t.convert: Name).show
          val tpeStr  = t.tpe.show
          s"$nameStr: $tpeStr"
        }
        val tpeStr = tpes.mkString("[", ", ", "]")
        fail(s"Typed successfully together as $tpeStr")
      }
  }

  def checkTpe(parsed: Checked[Tree], checkTpe: String): Unit = {
      parsed.fold
        { err => fail(err.show) }
        { tpd => assertEquals(checkTpe, tpd.tpe.show) }
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