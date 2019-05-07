package eec
package compiler
package parsing

import Parsers._
import ast.Trees.Tree
import error.CompilerErrors.Lifted
import core.Contexts.IdReader

object EntryPoint {
  val parseEEC: String => IdReader[Lifted[Tree]] =
    eecParser `toTreeParser` fromTranslationUnit

  val parseDef: String => IdReader[Lifted[Tree]] =
    statParser `toTreeParser` fromStatAsTop

  val parseExpr: String => IdReader[Lifted[Tree]] =
    exprParser `toTreeParser` fromExprAsTop
}