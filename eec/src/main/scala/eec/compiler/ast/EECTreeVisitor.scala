package eec
package compiler
package ast

import Trees._
import Trees.Tree._
import Names._
import Name._
import Constants._
import Constant._

import eec.compiler.exception._
import org.antlr.v4.runtime.tree.TerminalNode
import scala.collection.JavaConversions._

class EECTreeVisitor extends EECBaseVisitor[Tree] {

  override def visitLiteral(ctx: EECParser.LiteralContext): Literal =
    if ctx.IntegerLiteral != null then {
      new Literal(BigIntConstant(BigInt(ctx.IntegerLiteral.getText)))
    } else if ctx.FloatingPointLiteral != null then {
      new Literal(BigDecConstant(BigDecimal(ctx.FloatingPointLiteral.getText)))
    } else if ctx.BooleanLiteral != null then {
      val bool = ctx.BooleanLiteral.getText match {
        case "True" => true
        case _ => false
      }
      new Literal(BooleanConstant(bool))
    } else if ctx.CharacterLiteral != null then {
      val charStr = ctx.CharacterLiteral.getText
        .stripPrefix("'").stripSuffix("'")
      val char = if charStr.length == 0 then 0 else charStr.charAt(0)
      new Literal(CharConstant(char))
    } else { // StringLiteral
      val text = ctx.StringLiteral.getText
      val string =
        if text startsWith "\"\"\"" then
          text stripPrefix "\"\"\"" stripSuffix "\"\"\""
        else
          text stripPrefix "\"" stripSuffix "\""
      new Literal(StringConstant(string))
    }

  override def visitId(ctx: EECParser.IdContext): Ident = {
    import NameOps._
    new Ident(ctx.getText.asName)
  }

  override def visitAlphaId(ctx: EECParser.AlphaIdContext): Ident = {
    import NameOps._
    new Ident(ctx.getText.asName)
  }

  override def visitQualId(ctx: EECParser.QualIdContext): RefTree = {
    import scala.language.implicitConversions
    import NameOps._
    def listToRefId(lst: List[String]): RefTree = lst match {
      case n :: Nil => new Ident(n.asName)
      case n :: tail => new Select(listToRefId(tail), n.asName)
      case Nil => EmptyTree
    }
    listToRefId(ctx.id.reverse.map(_.getText).toList)
  }

  override def visitStableId(ctx: EECParser.StableIdContext): RefTree = {
    import scala.language.implicitConversions
    val ids = ctx.id.map(visitId).ensuring(_.size <= 2)
    if ids.size == 1 then
      ids(0)
    else
      new Select(ids(0), ids(1).name)
  }

  override def visitType(ctx: EECParser.TypeContext): TypeTree =
    if ctx.`type` != null then {
      val arg = visitInfixType(ctx.infixType)
      val body = visitType(ctx.`type`)
      new FunctionType(arg, body)
    } else {
      visitInfixType(ctx.infixType)
    }

  override def visitInfixType(ctx: EECParser.InfixTypeContext): TypeTree =
    if ctx.prefixType != null then
      visitPrefixType(ctx.prefixType)
    else
      visitProductType(ctx.productType)

  override def visitProductType(ctx: EECParser.ProductTypeContext): TypeTree = {
    import scala.language.implicitConversions
    import Bootstraps._
    val types = ctx.`type`
      .map(visitType)
      .ensuring(l => l.size == 0 || l.size == 2)
    val tag = if types.size == 0 then UnitTag else Tuple2Tag
    new TypeApply(new Ident(Bootstrapped(tag)), types.toList)
  }

  override def visitPrefixType(ctx: EECParser.PrefixTypeContext): TypeTree =
    if ctx.simpleType != null then {
      visitSimpleType(ctx.simpleType)
    } else {
      import Bootstraps._
      val tag = new Ident(Bootstrapped(ComputationTag))
      val args = visitType(ctx.`type`) :: Nil
      new TypeApply(tag, args)
    }

  override def visitSimpleType(ctx: EECParser.SimpleTypeContext): TypeTree =
    if ctx.qualId != null then
      visitQualId(ctx.qualId)
    else // must be type
      visitType(ctx.`type`)

  // override def visitAscription(ctx: EECParser.AscriptionContext): Tree =
  //   EmptyTree

  override def visitExpr(ctx: EECParser.ExprContext): ExprTree =
    if ctx.lambda != null then
      visitLambda(ctx.lambda)
    else if ctx.letExpr != null then
      visitLetExpr(ctx.letExpr)
    else if ctx.caseExpr != null then
      visitCaseExpr(ctx.caseExpr)
    else // must be expr1
      visitExpr1(ctx.expr1)

  override def visitLambda(ctx: EECParser.LambdaContext): Function = {
    import ValDefTreeOps._
    val bindings = visitBindings(ctx.bindings).toList
    val body = visitExpr(ctx.expr)
    new Function(bindings, body)
  }

  override def visitLetExpr(ctx: EECParser.LetExprContext): Let = {
    import NameOps._
    var name =
      if ctx.Varid != null then
        ctx.Varid.getText.asName
      else
        ctx.Wildcard.getText.asName
    var exprs = ctx.expr.ensuring(_.size == 2)
    var value = visitExpr(exprs.get(0))
    var continuation = visitExpr(exprs.get(1))
    new Let(name, value, continuation)
  }

  override def visitCaseExpr(ctx: EECParser.CaseExprContext): ExprTree = {
    val expr = visitExpr(ctx.expr)
    val cases = visitCases(ctx.cases).cases
    new CaseExpr(expr, cases)
  }

  override def visitExpr1(ctx: EECParser.Expr1Context): ExprTree =
    if ctx.infixExpr != null then {
      visitInfixExpr(ctx.infixExpr)
    } else {
      import scala.language.implicitConversions
      val exprs = ctx.expr
        .ensuring(_.size == 3)
        .map(visitExpr)
      new IfThenElse(exprs(0), exprs(1), exprs(2))
    }

  override def visitInfixExpr(ctx: EECParser.InfixExprContext): ExprTree =
    if ctx.prefixExpr != null then {
      visitPrefixExpr(ctx.prefixExpr)
    } else {
      val id =
        if ctx.OpId != null then {
          import NameOps._
          new Ident(ctx.OpId.getText.asName)
        } else {
          visitAlphaId(ctx.alphaId)
        }
      import scala.language.implicitConversions
      val infixes = ctx.infixExpr
        .ensuring(_.size == 2)
        .map(visitInfixExpr)
      new Apply(new Apply(id, infixes(0)), infixes(1))
    }

  override def visitPrefixExpr(ctx: EECParser.PrefixExprContext): ExprTree = {
    val simpleExpr = visitSimpleExpr(ctx.simpleExpr)
    if ctx.Bang != null then {
      import Bootstraps._
      val tag = new Ident(Bootstrapped(ComputationTag))
      new Apply(tag, simpleExpr)
    } else {
      simpleExpr
    }
  }

  override def visitSimpleExpr(ctx: EECParser.SimpleExprContext): ExprTree =
    if ctx.literal != null then
      visitLiteral(ctx.literal)
    else if ctx.stableId != null then
      visitStableId(ctx.stableId)
    else if ctx.exprsInParens != null then
      visitExprsInParens(ctx.exprsInParens)
    else {
      val simpleExpr = visitSimpleExpr(ctx.simpleExpr)
      val argExpr = visitArgumentExpr(ctx.argumentExpr)
      new Apply(simpleExpr, argExpr)
    }

  override def visitCases(ctx: EECParser.CasesContext): CaseClauses = {
    import scala.language.implicitConversions
    val caseClauses = ctx.caseClause.map(visitCaseClause).toList
    new CaseClauses(caseClauses)
  }

  override def visitCaseClause(ctx: EECParser.CaseClauseContext): CaseClause = {
    val pat = visitPattern(ctx.pattern)
    val guard =
      if ctx.guard != null then
        visitGuard(ctx.guard)
      else
        EmptyTree
    val body = visitExpr(ctx.expr)
    new CaseClause(pat, guard, body)
  }

  override def visitExprsInParens(
    ctx: EECParser.ExprsInParensContext): ExprTree = {
      val expr = {
        import scala.language.implicitConversions
        ctx.expr.map(visitExpr)
      }
      if expr.size == 1 then {
        expr(0)
      } else {
        import Bootstraps._
        assert(expr.size <= 2)
        if expr.size == 0 then {
          val tag = new Ident(Bootstrapped(UnitTag))
          new Apply(tag, EmptyTree)
        } else {
          val tag = new Ident(Bootstrapped(Tuple2Tag))
          new Apply(new Apply(tag, expr(0)), expr(1))
        }
      }
    }

  override def visitArgumentExpr(ctx: EECParser.ArgumentExprContext): ExprTree =
    if ctx.expr != null then
      visitExpr(ctx.expr)
    else
      EmptyTree

  override def visitPattern(ctx: EECParser.PatternContext): PatTree = {
    val patterns = {
      import scala.language.implicitConversions
      ctx.pattern1.map(visitPattern1)
    }
    if patterns.size == 1 then
      patterns(0)
    else
      new Alternative(patterns.toList)
  }

  override def visitPattern1(ctx: EECParser.Pattern1Context): PatTree =
    visitPattern2(ctx.pattern2)

  override def visitPattern2(ctx: EECParser.Pattern2Context): PatTree =
    if ctx.Varid != null then {
      val name = {
        import NameOps._
        ctx.Varid.getText.asName
      }
      if ctx.pattern3 != null then {
        new Bind(name, visitPattern3(ctx.pattern3))
      } else {
        new Ident(name)
      }
    } else {
      visitPattern3(ctx.pattern3)
    }

  override def visitPattern3(ctx: EECParser.Pattern3Context): PatTree =
    visitSimplePattern(ctx.simplePattern)

  override def visitSimplePattern(
    ctx: EECParser.SimplePatternContext): PatTree =
      if ctx.Wildcard != null then {
        wildcardIdent
      } else if ctx.getText == "()" then {
        import Bootstraps._
        val unitTag = new Ident(Bootstrapped(UnitTag))
        new Unapply(unitTag, Nil)
      } else if ctx.Varid != null then {
        import NameOps._
        new Ident(ctx.Varid.getText.asName)
      } else if ctx.literal != null then {
        visitLiteral(ctx.literal)
      } else if ctx.simplePattern != null then { // Bang present
        import Bootstraps._
        val simplePat = visitSimplePattern(ctx.simplePattern)
        val computation = new Ident(Bootstrapped(ComputationTag))
        new Unapply(computation, List(simplePat))
      } else { // tuple
        visitUpToPairPatten(ctx.upToPairPatten)
      }

  override def visitUpToPairPatten(
      ctx: EECParser.UpToPairPattenContext): PatTree = {
        val patterns = {
          import scala.language.implicitConversions
          ctx.pattern.map(visitPattern)
        }
        if patterns.size == 1 then {
          patterns(0)
        } else {
          import Bootstraps._
          assert(patterns.size == 2)
          val tuple2Tag = new Ident(Bootstrapped(Tuple2Tag))
          new Unapply(tuple2Tag, patterns.toList)
        }
      }

  override def visitGuard(ctx: EECParser.GuardContext): ExprTree =
    visitInfixExpr(ctx.infixExpr)

  override def visitBindings(ctx: EECParser.BindingsContext): ValDefTree =
    visitBindingsTagged(ctx.bindingsTagged)

  override def visitBindingsTagged(
      ctx: EECParser.BindingsTaggedContext): ValDefTree = {
        val bindings = {
          import scala.language.implicitConversions
          ctx.binding.map(visitBinding).toList
        }
        new Bindings(bindings)
      }

  override def visitBinding(ctx: EECParser.BindingContext): ValDefTree = {
    val name = visitId(ctx.id).name
    val typ = visitType(ctx.`type`)
    new Tagged(name, typ)
  }

  override def visitDcl(ctx: EECParser.DclContext): MemberDefTree =
    visitPrimitiveDcl(ctx.primitiveDcl)

  override def visitPrimitiveDcl(
    ctx: EECParser.PrimitiveDclContext): MemberDefTree = {
      import TreeOps._
      import Modifiers._
      visitDefDecl(ctx.defDecl).addModifiers(Set(Modifier.Primitive))
    }

  override def visitDefDecl(ctx: EECParser.DefDeclContext): MemberDefTree = {
    val sig = visitDefSig(ctx.defSig)
    val typ = visitType(ctx.`type`)
    new Def(Set(), sig, typ, EmptyTree)
  }

  override def visitDef(ctx: EECParser.DefContext): MemberDefTree =
    visitDefDef(ctx.defDef)

  override def visitDefDef(ctx: EECParser.DefDefContext): MemberDefTree = {
    val defSig = visitDefSig(ctx.defSig)
    val typ = visitType(ctx.`type`)
    val expr = visitExpr(ctx.expr)
    new Def(Set(), defSig, typ, expr)
  }

  override def visitDefSig(ctx: EECParser.DefSigContext): SigTree =
    if ctx.infixDefSig != null then {
      visitInfixDefSig(ctx.infixDefSig)
    } else {
      var varids = {
        import scala.language.implicitConversions
        import NameOps._
        ctx.Varid.map(_.getText.asName).toList
      }
      new DefSig(varids.head, varids.tail)
    }

  override def visitInfixDefSig(ctx: EECParser.InfixDefSigContext): SigTree =
    if ctx.prefixOpSig != null then {
      visitPrefixOpSig(ctx.prefixOpSig)
    } else if ctx.OpId != null then {
      import scala.language.implicitConversions
      import NameOps._
      var args = ctx.Varid
        .ensuring(_.size == 2)
        .map(_.getText.asName)
        .toList
      new DefSig(ctx.OpId.getText.asName, args)
    } else {
      import scala.language.implicitConversions
      import NameOps._
      var varids = ctx.Varid
        .ensuring(_.size == 3)
        .map(_.getText.asName)
      new DefSig(varids(1), List(varids(0), varids(2)))
    }

  override def visitPrefixOpSig(ctx: EECParser.PrefixOpSigContext): SigTree = {
    import NameOps._
    var args = {
      import scala.language.implicitConversions
      ctx.Varid.map(_.getText.asName).toList
    }
    new DefSig(ctx.OpId.getText.asName, args)
  }

  override def visitPackageInfo(ctx: EECParser.PackageInfoContext): RefTree =
    visitQualId(ctx.qualId)

  override def visitStatSeq(ctx: EECParser.StatSeqContext): Tree = {
    val stats = {
      import scala.language.implicitConversions
      ctx.stat.map(visitStat).toList
    }
    PackageDef(emptyIdent, stats)
  }

  override def visitStat(ctx: EECParser.StatContext): Tree =
    if ctx.`def` != null then visitDef(ctx.`def`) else visitDcl(ctx.dcl)

  override def visitTranslationUnit(
    ctx: EECParser.TranslationUnitContext): Tree = {
      val pkgId = visitPackageInfo(ctx.packageInfo)
      val stats = visitStatSeq(ctx.statSeq)
      import TreeOps._
      PackageDef(pkgId, stats.toList)
    }
}