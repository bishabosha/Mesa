package eec
package compiler
package types

import core.Contexts._
import parsing.parseExpr
import error.CompilerErrors._

object ExprBootstraps {

  def typecheck(seq: (String, String)*): Unit = {
    val (idGen, ctx)     = initialCtx
    delegate for IdGen   = idGen
    delegate for Context = ctx
    seq.toList.foreach { (f, s) => checkTpe(typeExpr(f)(any), s) }
  }

  def noType(seq: String*): Unit = {
    val (idGen, ctx)     = initialCtx
    delegate for IdGen   = idGen
    delegate for Context = ctx
    seq.toList.foreach { f => failIfUnparsedOrTypedExpr(f)(any) }
  }

  def noParse(seq: String*): Unit = {
    val (idGen, ctx)     = initialCtx
    delegate for IdGen   = idGen
    delegate for Context = ctx
    seq.toList.foreach { f => failIfParsed(parseExpr)(f) }
  }
}