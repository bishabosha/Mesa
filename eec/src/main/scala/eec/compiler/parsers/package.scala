package eec
package compiler
package object parsing {

  import ast.Trees._
  import Parsers._
  import Parsers.TreeParsers._
  import error.CompilerErrors._

  val parseEEC: String => Checked[Tree] =
    eecParser toTreeParser fromTranslationUnit

  val parseExpr: String => Checked[Tree] =
    exprParser toTreeParser fromExpr

  val parseType: String => Checked[Tree] =
    typeParser toTreeParser fromType
}