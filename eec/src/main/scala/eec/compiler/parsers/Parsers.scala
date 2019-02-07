package eec
package compiler
package parsers

object Parsers {

  import ast.Trees._
  import ast.Trees.Tree._
  import ast.Trees.untyped._
  import core.Names._
  import core.Constants._
  import core.Constants.Constant._
  import error.ParserErrors._
  import error.ParserErrors.ParserError._
  import org.antlr.v4.runtime.tree.TerminalNode
  import org.antlr.v4.runtime._
  import scala.collection.JavaConversions._

  private[Parsers] val eecErrorListener: BaseErrorListener = new {
    override def syntaxError(
      recognizer: Recognizer[_, _],
      offendingSymbol: AnyRef,
      line: Int,
      charPositionInLine: Int,
      msg: String,
      e: RecognitionException): Unit = {
        throw new ParserSyntaxException(
          "line " + line + ":" + charPositionInLine + " " + msg)
      }
  }

  private[Parsers] def parser(f: (EECTreeVisitor, EECParser) => Tree): (
    String => Tree | ParserError) =
      input => {
        val charStream = new ANTLRInputStream(input)
        val lexer = new EECLexer(charStream)
        val tokens = new CommonTokenStream(lexer)
        val parser = new EECParser(tokens)
        parser.removeErrorListeners
        parser.addErrorListener(eecErrorListener)
        f(new EECTreeVisitor(), parser).recover
      }

  private[parsers] val eecParser = parser { (v, p) => (
    v.visitTranslationUnit(p.translationUnit())
  )}

  private[parsers] val exprParser = parser((v, p) => v.visitExpr(p.expr()))

  private[parsers] class EECTreeVisitor extends EECBaseVisitor[Tree] {

    override def visitLiteral(ctx: EECParser.LiteralContext): Tree =
      if ctx.IntegerLiteral != null then {
        Literal(uTpe, BigIntConstant(BigInt(ctx.IntegerLiteral.getText)))
      } else if ctx.FloatingPointLiteral != null then {
        Literal(uTpe, BigDecConstant(BigDecimal(ctx.FloatingPointLiteral.getText)))
      } else if ctx.BooleanLiteral != null then {
        val bool = ctx.BooleanLiteral.getText match {
          case "True" => true
          case _ => false
        }
        Literal(uTpe, BooleanConstant(bool))
      } else if ctx.CharacterLiteral != null then {
        val charStr = ctx.CharacterLiteral.getText
          .stripPrefix("'").stripSuffix("'")
        val char = if charStr.length == 0 then 0 else charStr.charAt(0)
        Literal(uTpe, CharConstant(char))
      } else { // StringLiteral
        val text = ctx.StringLiteral.getText
        val string =
          if text startsWith "\"\"\"" then
            text stripPrefix "\"\"\"" stripSuffix "\"\"\""
          else
            text stripPrefix "\"" stripSuffix "\""
        Literal(uTpe, StringConstant(string))
      }

    override def visitId(ctx: EECParser.IdContext): Tree = {
      import NameOps._
      Ident(uTpe, ctx.getText.readAs)
    }

    override def visitAlphaId(ctx: EECParser.AlphaIdContext): Tree = {
      import NameOps._
      Ident(uTpe, ctx.getText.readAs)
    }

    override def visitQualId(ctx: EECParser.QualIdContext): Tree = {
      import scala.language.implicitConversions
      import NameOps._
      def listToRefId(lst: List[String]): Tree = lst match {
        case n :: Nil => Ident(uTpe, n.readAs)
        case n :: tail => Select(uTpe, listToRefId(tail), n.readAs)
        case Nil => EmptyTree
      }
      listToRefId(ctx.id.reverse.map(_.getText).toList)
    }

    override def visitStableId(ctx: EECParser.StableIdContext): Tree = {
      val ids = {
        import scala.language.implicitConversions
        ctx.id.map(visitId).ensuring(_.size <= 2)
      }
      if ids.size == 1 then {
        ids(0)
      } else {
        import TreeOps._
        val src = ids(0)
        val List(select) = ids(1).toNames
        Select(uTpe, src, select)
      }
    }

    override def visitType(ctx: EECParser.TypeContext): Tree =
      if ctx.`type` != null then {
        val arg = visitInfixType(ctx.infixType)
        val body = visitType(ctx.`type`)
        Function(uTpe, List(arg), body)
      } else {
        visitInfixType(ctx.infixType)
      }

    override def visitInfixType(ctx: EECParser.InfixTypeContext): Tree =
      if ctx.prefixType != null then
        visitPrefixType(ctx.prefixType)
      else
        visitProductType(ctx.productType)

    override def visitProductType(ctx: EECParser.ProductTypeContext): Tree = {
      import scala.language.implicitConversions
      val types = ctx.`type`
        .map(visitType)
        .ensuring(l => l.size == 0 || l.size == 2)
      Parens(uTpe, types.toList)
    }

    override def visitPrefixType(ctx: EECParser.PrefixTypeContext): Tree =
      if ctx.simpleType != null then {
        visitSimpleType(ctx.simpleType)
      } else {
        val tag = Ident(uTpe, Name.ComputationTag)
        val args = visitType(ctx.`type`) :: Nil
        Apply(uTpe, tag, args)
      }

    override def visitSimpleType(ctx: EECParser.SimpleTypeContext): Tree =
      if ctx.qualId != null then
        visitQualId(ctx.qualId)
      else
        visitType(ctx.`type`)

    override def visitExpr(ctx: EECParser.ExprContext): Tree =
      if ctx.lambda != null then
        visitLambda(ctx.lambda)
      else if ctx.letExpr != null then
        visitLetExpr(ctx.letExpr)
      else if ctx.caseExpr != null then
        visitCaseExpr(ctx.caseExpr)
      else if ctx.expr1 != null then
        visitExpr1(ctx.expr1)
      else
        visitExprSeqAsApply(ctx.expr)
    
    def visitExprSeqAsApply(
      exprs: java.util.List[EECParser.ExprContext]): Tree = {
        import scala.language.implicitConversions
        import TreeOps._
        val Seq(expr, arg) = exprs.map(visitExpr).ensuring(_.size == 2)
        Apply(uTpe, expr, arg.toList)
      }

    override def visitLambda(ctx: EECParser.LambdaContext): Tree = {
      import TreeOps._
      val bindings = visitBindings(ctx.bindings).toList
      val body = visitExpr(ctx.expr)
      Function(uTpe, bindings, body)
    }

    override def visitLetExpr(ctx: EECParser.LetExprContext): Tree = {
      import NameOps._
      var name =
        if ctx.Varid != null then
          ctx.Varid.getText.readAs
        else
          ctx.Wildcard.getText.readAs
      var exprs = ctx.expr.ensuring(_.size == 2)
      var value = visitExpr(exprs.get(0))
      var continuation = visitExpr(exprs.get(1))
      Let(uTpe, name, value, continuation)
    }

    override def visitCaseExpr(ctx: EECParser.CaseExprContext): Tree = {
      import TreeOps._
      val expr = visitExpr(ctx.expr)
      val cases = visitCases(ctx.cases).toList
      CaseExpr(uTpe, expr, cases)
    }

    override def visitExpr1(ctx: EECParser.Expr1Context): Tree =
      if ctx.infixExpr != null then {
        visitInfixExpr(ctx.infixExpr)
      } else {
        import scala.language.implicitConversions
        val exprs = ctx.expr
          .ensuring(_.size == 3)
          .map(visitExpr)
        If(uTpe, exprs(0), exprs(1), exprs(2))
      }

    override def visitInfixExpr(ctx: EECParser.InfixExprContext): Tree =
      if ctx.prefixExpr != null then {
        visitPrefixExpr(ctx.prefixExpr)
      } else {
        val id =
          if ctx.OpId != null then {
            import NameOps._
            Ident(uTpe, ctx.OpId.getText.readAs)
          } else {
            visitAlphaId(ctx.alphaId)
          }
        import scala.language.implicitConversions
        val infixes = ctx.infixExpr
          .ensuring(_.size == 2)
          .map(visitInfixExpr)
        val firstApply = Apply(uTpe, id, List(infixes(0)))
        Apply(uTpe, firstApply, List(infixes(1)))
      }

    override def visitPrefixExpr(ctx: EECParser.PrefixExprContext): Tree = {
      val simpleExpr = visitSimpleExpr(ctx.simpleExpr)
      if ctx.Bang != null then {
        import NameOps._
        val tag = Ident(uTpe, Name.ComputationTag)
        Apply(uTpe, tag, List(simpleExpr))
      } else {
        simpleExpr
      }
    }

    override def visitSimpleExpr(ctx: EECParser.SimpleExprContext): Tree =
      if ctx.literal != null then
        visitLiteral(ctx.literal)
      else if ctx.stableId != null then
        visitStableId(ctx.stableId)
      else
        visitExprsInParens(ctx.exprsInParens)

    override def visitCases(ctx: EECParser.CasesContext): Tree = {
      import scala.language.implicitConversions
      val caseClauses = ctx.caseClause.map(visitCaseClause).toList
      TreeSeq(caseClauses)
    }

    override def visitCaseClause(ctx: EECParser.CaseClauseContext): Tree = {
      val pat = visitPattern(ctx.pattern)
      val guard =
        if ctx.guard != null then
          visitGuard(ctx.guard)
        else
          EmptyTree
      val body = visitExpr(ctx.expr)
      CaseClause(uTpe, pat, guard, body)
    }

    override def visitExprsInParens(
      ctx: EECParser.ExprsInParensContext): Tree = {
        val expr = {
          import scala.language.implicitConversions
          ctx.expr.map(visitExpr)
        }
        if expr.size == 1 then
          expr(0)
        else
          Parens(uTpe, expr.toList)
      }

    override def visitPattern(ctx: EECParser.PatternContext): Tree = {
      val patterns = {
        import scala.language.implicitConversions
        ctx.pattern1.map(visitPattern1)
      }
      if patterns.size == 1 then
        patterns(0)
      else
        Alternative(uTpe, patterns.toList)
    }

    override def visitPattern1(ctx: EECParser.Pattern1Context): Tree =
      visitPattern2(ctx.pattern2)

    override def visitPattern2(ctx: EECParser.Pattern2Context): Tree =
      if ctx.Varid != null then {
        val name = {
          import NameOps._
          ctx.Varid.getText.readAs
        }
        if ctx.pattern3 != null then {
          Bind(uTpe, name, visitPattern3(ctx.pattern3))
        } else {
          Ident(uTpe, name)
        }
      } else {
        visitPattern3(ctx.pattern3)
      }

    override def visitPattern3(ctx: EECParser.Pattern3Context): Tree =
      visitSimplePattern(ctx.simplePattern)

    override def visitSimplePattern(
      ctx: EECParser.SimplePatternContext): Tree =
        if ctx.Wildcard != null then {
          wildcardIdent
        } else if ctx.getText == "()" then {
          Parens(uTpe, Nil)
        } else if ctx.Varid != null then {
          import NameOps._
          Ident(uTpe, ctx.Varid.getText.readAs)
        } else if ctx.literal != null then {
          visitLiteral(ctx.literal)
        } else if ctx.simplePattern != null then { // Bang present
          val simplePat = visitSimplePattern(ctx.simplePattern)
          val computation = Ident(uTpe, Name.ComputationTag)
          Unapply(uTpe, computation, List(simplePat))
        } else { // tuple
          visitUpToPairPatten(ctx.upToPairPatten)
        }

    override def visitUpToPairPatten(
        ctx: EECParser.UpToPairPattenContext): Tree = {
          val patterns = {
            import scala.language.implicitConversions
            ctx.pattern.map(visitPattern)
          }
          if patterns.size == 1 then {
            patterns(0)
          } else {
            assert(patterns.size == 2)
            Parens(uTpe, patterns.toList)
          }
        }

    override def visitGuard(ctx: EECParser.GuardContext): Tree =
      visitInfixExpr(ctx.infixExpr)

    override def visitBindings(ctx: EECParser.BindingsContext): Tree =
      visitBindingsTagged(ctx.bindingsTagged)

    override def visitBindingsTagged(
      ctx: EECParser.BindingsTaggedContext): Tree = {
        import scala.language.implicitConversions
        val bindings = ctx.binding.map(visitBinding).toList
        TreeSeq(bindings)
      }

    override def visitBinding(ctx: EECParser.BindingContext): Tree = {
      import TreeOps._
      val List(name) = visitId(ctx.id).toNames
      val typ = visitType(ctx.`type`)
      Tagged(uTpe, name, typ)
    }

    override def visitDcl(ctx: EECParser.DclContext): Tree =
      visitPrimitiveDcl(ctx.primitiveDcl)

    override def visitPrimitiveDcl(
      ctx: EECParser.PrimitiveDclContext): Tree = {
        import TreeOps._
        import NameOps._
        import eec.compiler.core.Modifiers._
        val modifiers: Set[Modifier] = Set(Modifier.Primitive)
        visitDefDecl(ctx.defDecl).addModifiers(modifiers)
      }

    override def visitDefDecl(ctx: EECParser.DefDeclContext): Tree = {
      val sig = visitDefSig(ctx.defSig)
      val typ = visitType(ctx.`type`)
      DefDef(uTpe, Set(), sig, typ, EmptyTree)
    }

    override def visitDef(ctx: EECParser.DefContext): Tree =
      visitDefDef(ctx.defDef)

    override def visitDefDef(ctx: EECParser.DefDefContext): Tree = {
      val defSig = visitDefSig(ctx.defSig)
      val typ = visitType(ctx.`type`)
      val expr = visitExpr(ctx.expr)
      DefDef(uTpe, Set(), defSig, typ, expr)
    }

    override def visitDefSig(ctx: EECParser.DefSigContext): Tree =
      if ctx.infixDefSig != null then {
        visitInfixDefSig(ctx.infixDefSig)
      } else {
        var varids = {
          import scala.language.implicitConversions
          import NameOps._
          ctx.Varid.map(_.getText.readAs).toList
        }
        DefSig(uTpe, varids.head, varids.tail)
      }

    override def visitInfixDefSig(ctx: EECParser.InfixDefSigContext): Tree = {
      import scala.language.implicitConversions
      import NameOps._
      if ctx.prefixOpSig != null then {
        visitPrefixOpSig(ctx.prefixOpSig)
      } else if ctx.OpId != null then {
        var args = ctx.Varid
          .ensuring(_.size == 2)
          .map(_.getText.readAs)
          .toList
        DefSig(uTpe, ctx.OpId.getText.readAs, args)
      } else {
        var varids = ctx.Varid
          .ensuring(_.size == 3)
          .map(_.getText.readAs)
        DefSig(uTpe, varids(1), List(varids(0), varids(2)))
      }
    }

    override def visitPrefixOpSig(ctx: EECParser.PrefixOpSigContext): Tree = {
      import scala.language.implicitConversions
      import NameOps._
      var args = ctx.Varid.map(_.getText.readAs).toList
      DefSig(uTpe, ctx.OpId.getText.readAs, args)
    }

    override def visitPackageInfo(ctx: EECParser.PackageInfoContext): Tree =
      visitQualId(ctx.qualId)

    override def visitStatSeq(ctx: EECParser.StatSeqContext): Tree = {
      val stats = {
        import scala.language.implicitConversions
        ctx.stat.map(visitStat).toList
      }
      TreeSeq(stats)
    }

    override def visitStat(ctx: EECParser.StatContext): Tree =
      if ctx.`def` != null then visitDef(ctx.`def`) else visitDcl(ctx.dcl)

    override def visitTranslationUnit(
      ctx: EECParser.TranslationUnitContext): Tree = {
        import TreeOps._
        val pkgId = visitPackageInfo(ctx.packageInfo)
        val stats = visitStatSeq(ctx.statSeq)
        PackageDef(uTpe, pkgId, stats.toList)
      }
  }
}