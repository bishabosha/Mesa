package eec
package compiler
package ast

import EEC._
import Fixity._
import eec.compiler.exception._
import scala.collection.JavaConversions._
import scala.language.implicitConversions

object EECAstVisitor extends EECBaseVisitor[Any] {

  override def visitLiteral
    (ctx: EECParser.LiteralContext): Literals =
      IntegerLiteral(ctx.IntegerLiteral.getText.toInt)

  override def visitPrimary
    (ctx: EECParser.PrimaryContext): PrimaryExpr =
      PrimaryExpr(visitExpr(ctx.expr))

  override def visitQualId
    (ctx: EECParser.QualIdContext): String =
      ctx.children.map(_.getText).mkString

  override def visitExpr
    (ctx: EECParser.ExprContext): Expr = {
      val first =
        Option(ctx.literal).map[Expressions](visitLiteral)
          .orElse[Expressions] { Option(ctx.primary).map(visitPrimary) }
          .orElse[Expressions] { Option(ctx.prefixExpr).map(visitPrefixExpr) }
      val start: Int = first.fold(0)(_ => 1)
      ctx.children.toList.foldLeft(Nil: Expr){ (acc, o) =>
        o.accept(this) match {
          case l: List[_] => l.asInstanceOf[Expr] ++ acc
          case o => o.asInstanceOf[Expressions] :: acc
        }
      }.reverse
    }

  override def visitOperator(ctx: EECParser.OperatorContext): Operator =
    Operator(ctx.OPERATOR.getText)

  override def visitPrefixExpr
    (ctx: EECParser.PrefixExprContext): PrefixExpr =
      PrefixExpr(Operator(ctx.OPERATOR.getText), visitExpr(ctx.expr))

  override def visitFixity
    (ctx: EECParser.FixityContext): List[FixityStatement] = {
      val strength = ctx.IntegerLiteral.getText.toInt
      val fixity = ctx.FIXITY.getText match {
        case "infix" => Infix(strength)
        case "postfix" => Postfix(strength)
        case "infixl" => Infixl(strength)
        case "infixr" => Infixr(strength)
        case "prefix" => Prefix(strength)
      }
      ctx.OPERATOR.view.map(o => FixityStatement(fixity,Operator(o.getText))).toList
    }

  override def visitModuleInfo
    (ctx: EECParser.ModuleInfoContext): ModuleInfo =
      ModuleInfo(visitQualId(ctx.qualId))

  override def visitTopStatSeq
    (ctx: EECParser.TopStatSeqContext): List[TopStatement] =
      ctx.topStat.toList.flatMap[TopStatement, List[TopStatement]](visitTopStat)

  override def visitTopStat
    (ctx: EECParser.TopStatContext): List[TopStatement] =
      Option(ctx.fixity).map(visitFixity)
        .orElse {
          Option(ctx.expr).map(visitExpr).map(List(_))
        }
        .getOrElse(Nil)

  override def visitTranslationUnit
    (ctx: EECParser.TranslationUnitContext): Ast = {
      val modules =
        for (m <- Option(ctx.moduleInfo)) yield m.toList.map(visitModuleInfo)
      val topStats =
        for (t <- Option(ctx.topStatSeq)) yield visitTopStatSeq(t)
      (modules.getOrElse(Nil), topStats.getOrElse(Nil))
    }
  
}