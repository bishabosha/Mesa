package eec
package compiler
package ast

import EEC._
import eec.compiler.exception._

class EECAstVisitor extends EECBaseVisitor[Any] {

  override def visitLiteral
    (ctx: EECParser.LiteralContext): Literals =
      IntegerLiteral(ctx.IntegerLiteral.getText.toInt)

  override def visitPrimary
    (ctx: EECParser.PrimaryContext): PrimaryExpr =
      PrimaryExpr(visitExpr(ctx.expr))

  override def visitQualId
    (ctx: EECParser.QualIdContext): String =
      visitChildren(ctx)

  override def visitExpr
    (ctx: EECParser.ExprContext): Expr =
      visitChildren(ctx)

  override def visitPrefixExpr
    (ctx: EECParser.PrefixExprContext): PrefixExpr =
      visitChildren(ctx)

  override def visitFixity
    (ctx: EECParser.FixityContext): Fixity =
      visitChildren(ctx)

  override def visitModuleInfo
    (ctx: EECParser.ModuleInfoContext): ModuleInfo =
      visitChildren(ctx)

  override def visitTopStatSeq
    (ctx: EECParser.TopStatSeqContext): List[TopStatement] =
      visitChildren(ctx)

  override def visitTopStat
    (ctx: EECParser.TopStatContext): TopStatement =
      visitChildren(ctx)

  override def visitTranslationUnit
    (ctx: EECParser.TranslationUnitContext): Ast =
      visitChildren(ctx)
  
}