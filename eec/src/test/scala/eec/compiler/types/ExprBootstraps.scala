package eec
package compiler
package types

import core.Contexts._
import ast.Trees._
import parsing.EntryPoint.parseExpr
import error.CompilerErrors._
import error.CompilerErrors.CompilerError._
import Types._
import types.Typers._
import error.CompilerErrors._
import BootstrapTests._
import CompilerErrorOps._

import org.junit.Test
import org.junit.Assert._

object ExprBootstraps {

  def typecheck(seq: (String, String)*): Unit = {
    val (idGen, ctx)    = initialCtx
    implied for IdGen   = idGen
    implied for Context = ctx
    seq.toList.foreach { (f, s) => checkTpe(typeExpr(f)(any), s) }
  }

  def noType(seq: String*): Unit = {
    val (idGen, ctx)    = initialCtx
    implied for IdGen   = idGen
    implied for Context = ctx
    seq.toList.foreach { f => failIfUnparsedOrTypedExpr(f)(any) }
  }

  def noParse(seq: String*): Unit = {
    val (idGen, ctx)    = initialCtx
    implied for IdGen   = idGen
    implied for Context = ctx
    seq.toList.foreach { f => failIfParsed(parseExpr)(f) }
  }
}