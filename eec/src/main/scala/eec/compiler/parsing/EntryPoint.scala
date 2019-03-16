package eec
package compiler
package parsing

import ast.Trees._
import Parsers._
import error.CompilerErrors._
import core.Contexts._

object EntryPoint {

  val parseEEC: String => IdMaker[Checked[Tree]] =
    eecParser `toTreeParser` fromTranslationUnit

  val parseStat: String => IdMaker[Checked[Tree]] =
    statParser `toTreeParser` fromStatAsTop

  val parseExpr: String => IdMaker[Checked[Tree]] =
    exprParser `toTreeParser` fromExprAsTop

}