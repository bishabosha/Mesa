package eec
package compiler
package types

import core.Contexts._
import ast.Trees._
import parsing._
import EntryPoint.parseDef
import error.CompilerErrors._
import error.CompilerErrors.CompilerError._
import Types._
import types.Typers._
import error.CompilerErrors._
import BootstrapTests._
import CompilerErrorOps._

import org.junit.Test
import org.junit.Assert._

object StatBootstraps {

  def typecheck(seq: (String, String)*): Unit = {
    val (idGen, ctx)    = initialCtx
    delegate for IdGen   = idGen
    delegate for Context = ctx
    seq.foreach { (f, s) => checkTpe(typeStat(f)(any), s) }
  }

  def noType(seq: String*): Unit = {
    val (idGen, ctx)    = initialCtx
    delegate for IdGen   = idGen
    delegate for Context = ctx
    seq.foreach { f => failIfUnparsedOrTypedStat(f)(any) }
  }

  def someNoType(seq: String*): Unit = {
    val (idGen, ctx)    = initialCtx
    delegate for IdGen   = idGen
    delegate for Context = ctx
    failIfAllTyped(seq.mapE { f => typeStat(f)(any) })
  }

  def noParse(seq: String*): Unit = {
    val (idGen, ctx)    = initialCtx
    delegate for IdGen   = idGen
    delegate for Context = ctx
    seq.toList.foreach { f => failIfParsed(parseDef)(f) }
  }
}