package eec
package compiler
package parsing

import Parsers._
import ast.Trees.Tree
import error.CompilerErrors.Checked
import core.Contexts.IdReader

object EntryPoint {
  val parseEEC: String => IdReader[Checked[Tree]] =
    eecParser `toTreeParser` fromTranslationUnit

  val parseStat: String => IdReader[Checked[Tree]] =
    statParser `toTreeParser` fromStatAsTop

  val parseExpr: String => IdReader[Checked[Tree]] =
    exprParser `toTreeParser` fromExprAsTop
}