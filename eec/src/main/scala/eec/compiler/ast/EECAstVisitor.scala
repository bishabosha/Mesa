package eec
package compiler
package ast

import EEC._
import Fixity._
import eec.compiler.exception._
import scala.collection.JavaConversions._
import scala.util.Try
import scala.language.implicitConversions

object EECAstVisitor extends EECBaseVisitor[Any] {

  override def visitLiteral
    (ctx: EECParser.LiteralContext): Literals =
      if ctx.IntegerLiteral != null then {
        val text = ctx.IntegerLiteral.getText
        val negate = ctx.SUB != null
        if text.last.toUpper == 'L' then
          Try(ctx.IntegerLiteral.getText.toLong)
            .map(l => if negate then l * -1 else l)
            .map(LongLiteral(_))
            .getOrElse{
              throw UnexpectedInput(ctx.getText)
            }
        else
          Try(ctx.IntegerLiteral.getText.toInt)
            .map(l => if negate then l * -1 else l)
            .map(IntegerLiteral(_))
            .getOrElse{
              throw UnexpectedInput(ctx.getText)
            }
      } else {
        val text = ctx.FloatingPointLiteral.getText
        val negate = ctx.SUB != null
        if text.last.toUpper == 'F' then
          Try(ctx.FloatingPointLiteral.getText.toFloat)
            .map(l => if negate then l * -1 else l)
            .map(FloatLiteral(_))
            .getOrElse{
              throw UnexpectedInput(ctx.getText)
            }
        else
          Try(ctx.FloatingPointLiteral.getText.toDouble)
            .map(l => if negate then l * -1 else l)
            .map(DoubleLiteral(_))
            .getOrElse{
              throw UnexpectedInput(ctx.getText)
            }
      }

  override def visitPrimary
    (ctx: EECParser.PrimaryContext): Expression =
      visitExpr(ctx.expr)

  override def visitTuple
    (ctx: EECParser.TupleContext): TupleExpr =
      TupleExpr(ctx.expr.toVector.map[Expression, Vector[Expression]](visitExpr))

  override def visitQualId
    (ctx: EECParser.QualIdContext): String =
      ctx.children.map(_.getText).mkString

  override def visitExpr
    (ctx: EECParser.ExprContext): Expression = {
      val first =
        Option(ctx.literal).map[Expressions](visitLiteral)
          .orElse[Expressions] { Option(ctx.primary).map(visitPrimary) }
          .orElse[Expressions] { Option(ctx.prefixExpr).map(visitPrefixExpr) }
          .orElse[Expressions] { Option(ctx.tuple).map(visitTuple) }
      val start: Int = first.fold(0)(_ => 1)
      val exprs = ctx.children.toList.foldLeft(Nil: List[Expressions]){ (acc, o) =>
        o.accept(this) match {
          case o => o.asInstanceOf[Expressions] :: acc
        }
      }.reverse
      exprs match {
        case List(l: Literals) => l
        case List(p: PrefixExpr) => p
        case List(e: Expr) => e
        case List(t: TupleExpr) => t
        case _ => Expr(exprs)
      }
    }

  override def visitOperator(ctx: EECParser.OperatorContext): Operator =
    Operator(Option(ctx.OPERATOR).getOrElse(ctx.SUB).getText)

  override def visitPrefixExpr
    (ctx: EECParser.PrefixExprContext): PrefixExpr =
      PrefixExpr(Operator(ctx.OPERATOR.getText), visitExpr(ctx.expr))

  override def visitFixity
    (ctx: EECParser.FixityContext): List[FixityStatement] = {
      val strength = ctx.IntegerLiteral.getText.toInt
      val fixity = ctx.FIXITY.getText match {
        case "prefix"   => Prefix
        case "infix"    => Infix
        case "infixl"   => Infixl
        case "infixr"   => Infixr
        case "postfix"  => Postfix
      }
      ctx.OPERATOR.view.map { o =>
        FixityStatement(fixity, strength, Operator(o.getText))
      }
      .toList ++ ctx.SUB.view.map { o =>
        FixityStatement(fixity, strength, Operator(o.getText))
      }
    }

  override def visitModuleInfo
    (ctx: EECParser.ModuleInfoContext): ModuleInfo =
      ModuleInfo(visitQualId(ctx.qualId))

  override def visitTopStatSeq
    (ctx: EECParser.TopStatSeqContext): List[TopStatement] =
      ctx.topStat.toList.flatMap[TopStatement, List[TopStatement]](visitTopStat)

  override def visitTopStat
    (ctx: EECParser.TopStatContext): List[TopStatement] =
      Option(ctx.fixity).map(visitFixity).getOrElse(Nil)

  override def visitStatSeq
    (ctx: EECParser.StatSeqContext): List[Statement] =
      ctx.stat.toList.flatMap[Statement, List[Statement]](visitStat)

  override def visitStat
    (ctx: EECParser.StatContext): Option[Statement] =
      Option(ctx.expr).map(visitExpr)

  override def visitTranslationUnit
    (ctx: EECParser.TranslationUnitContext): Ast = {
      val modules =
        for (m <- Option(ctx.moduleInfo)) yield m.toList.map(visitModuleInfo)
      val topStats =
        for (t <- Option(ctx.topStatSeq)) yield visitTopStatSeq(t)
      val stats =
        for (t <- Option(ctx.statSeq)) yield visitStatSeq(t)
      (modules.getOrElse(Nil), topStats.getOrElse(Nil), stats.getOrElse(Nil))
    }
  
}