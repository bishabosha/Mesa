package mesa
package compiler
package types

import scala.language.implicitConversions

import core.Contexts._
import core.Names._
import error.CompilerErrors._
import parsing.{parseDef, parseExpr}
import Typers._
import Namers._
import CompilerErrorOps._
import Namers._
import Types._
import Type._
import ast.Trees._

import org.junit.Assert._

import CompilerErrorOps.given
import TreeOps.given
import TypeOps.given
import NameOps.given

val any = WildcardType

def (str: String) -|: (other: String) = other -> str
def (str: String) :|- (other: String) = str -> other

def typeExpr: String => Type => Contextual[IdReader[Lifted[Tree]]] =
  typeCore(parseExpr)

def typeStat: String => Type => Contextual[IdReader[Lifted[Tree]]] =
  typeCore(parseDef)

def failIfUnparsedOrTypedExpr: String => Type => Contextual[IdReader[Unit]] =
  failIfUnparsedOrTyped(parseExpr)

def failIfUnparsedOrTypedStat: String => Type => Contextual[IdReader[Unit]] =
  failIfUnparsedOrTyped(parseDef)

def typeCore(f: String => IdReader[Lifted[Tree]])
            (str: String)
            (pt: Type)
            (using IdGen, Context): Lifted[Tree] =
  for
    exp <- f(str)
    tpd <- typeAfterParse(pt)(exp)
  yield tpd

def typeAfterParse(pt: Type)
                  (exp: Tree)
                  (using IdGen, Context): Lifted[Tree] =
  for
    _   <- exp.indexed
    tpd <- exp.typed
  yield tpd

def failIfUnparsedOrTyped(f: String => IdReader[Lifted[Tree]])
                         (str: String)
                         (pt: Type)
                         (using IdGen, Context): Unit = {
  f(str).fold
    { err => fail(err.show) }
    { tpd =>
      failIfTyped(typeAfterParse(pt)(tpd))
    }
}

def failIfParsed(f: String => IdReader[Lifted[Tree]])
                (str: String)
                (using IdGen): Unit = {
  f(str).fold
    { err => () }
    { exp => fail(s"Parsed expr sucessfully:\n${exp.show}") }
}

def failIfTyped(tpd: Lifted[Tree]): Unit = {
  tpd.fold
    { err => () }
    { tpd =>
      val tpeStr = tpd.tpe.show
      val tpdStr = tpd.show
      fail(s"Typed successfully as $tpeStr for expr:\n$tpdStr")
    }
}

def failIfAllTyped(tpd: Lifted[Iterable[Tree]]): Unit = {
  tpd.fold
    { err => () }
    { tpds =>
      val tpes = tpds.map { t =>
        val nameStr = (t: Name).show
        val tpeStr  = t.tpe.show
        s"$nameStr: $tpeStr"
      }
      val tpeStr = tpes.mkString("[", ", ", "]")
      fail(s"Typed successfully together as $tpeStr")
    }
}

def checkTpe(parsed: Lifted[Tree], checkTpe: String): Unit = {
  parsed.fold
    { err => fail(err.show) }
    { tpd => assertEquals(checkTpe, tpd.tpe.show) }
}

def initialCtx: (IdGen, Context) = {

  val idGen = new IdGen
  val ctx   = new RootContext()
  given Context = ctx
  given IdGen   = idGen

  val pair =
    for
      _ <- Context.enterBootstrapped
      _ <- Prelude.source.mapE(typeStat(_)(any))
    yield (idGen, ctx)

  pair.onError { err => fail(s"[INIT] ${err.show}"); ??? }
}
