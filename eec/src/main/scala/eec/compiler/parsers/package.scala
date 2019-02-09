package eec
package compiler
package object parsers {

  import ast.Trees._
  import Parsers._
  import Parsers.TreeParsers._
  import error.ParserErrors._

  class ParserSyntaxException(msg: String) extends Exception(msg)

  val parseEEC: String => Tree | ParserError =
    eecParser toTreeParser fromTranslationUnit

  val parseExpr: String => Tree | ParserError =
    exprParser toTreeParser fromExpr
}