package eec
package compiler
package object parsers {

  import ast.Trees._
  import Parsers._
  import errors.ParserErrors._

  class ParserSyntaxException(msg: String) extends Exception(msg)

  def parseEEC(input: String): Tree | ParserError = eecParser(input)

  def parseExpr(input: String): Tree | ParserError = exprParser(input)
}