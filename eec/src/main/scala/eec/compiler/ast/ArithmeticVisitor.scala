package eec
package compiler
package ast

import Arithmetic._
import eec.compiler.exception._
import scala.util.Try

object ArithmeticVisitor extends ArithmeticParserBaseVisitor[Arithmetic[_]] {

  override def visitTranslationUnit(ctx: ArithmeticParser.TranslationUnitContext): Arithmetic[Double] = {
    if ctx.expr() != null then {
      visitExpr(ctx.expr())
    } else if ctx.NUMBER() != null then {
      parseNumber(ctx.NUMBER().getText)
    } else {
      throw UnexpectedEOF
    }
  }

  override def visitExpr(ctx: ArithmeticParser.ExprContext): Arithmetic[Double] = {
    import scala.collection.JavaConversions._
    import scala.language.implicitConversions

    val operands = ctx.NUMBER().toList.map(_.getText)
    val operand1 = parseNumber(operands.lift(0).getOrElse("0.0"))
    val operand2 = parseNumber(operands.lift(1).getOrElse("0.0"))

    val operation = visitOperation(ctx.operation())

    Expr(operation, operand1, operand2)
  }

  def parseNumber(s: String): Arithmetic[Double] = Number(Try(s.toDouble).toOption.getOrElse(0.0))

  override def visitOperation(ctx: ArithmeticParser.OperationContext): Arithmetic[Op[Double]] = {
    val op = ctx.getText
    Op(op)
  }
}