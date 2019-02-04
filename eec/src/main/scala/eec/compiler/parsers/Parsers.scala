package eec
package compiler
package parsers

object Parsers {

  import ast.Trees._
  import ast.Trees.Tree._
  import core.Names._
  import core.Names.Name._
  import core.Constants._
  import core.Constants.Constant._
  import errors.ParserErrors._
  import errors.ParserErrors.ParserError._
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
        Literal(BigIntConstant(BigInt(ctx.IntegerLiteral.getText)))
      } else if ctx.FloatingPointLiteral != null then {
        Literal(BigDecConstant(BigDecimal(ctx.FloatingPointLiteral.getText)))
      } else if ctx.BooleanLiteral != null then {
        val bool = ctx.BooleanLiteral.getText match {
          case "True" => true
          case _ => false
        }
        Literal(BooleanConstant(bool))
      } else if ctx.CharacterLiteral != null then {
        val charStr = ctx.CharacterLiteral.getText
          .stripPrefix("'").stripSuffix("'")
        val char = if charStr.length == 0 then 0 else charStr.charAt(0)
        Literal(CharConstant(char))
      } else { // StringLiteral
        val text = ctx.StringLiteral.getText
        val string =
          if text startsWith "\"\"\"" then
            text stripPrefix "\"\"\"" stripSuffix "\"\"\""
          else
            text stripPrefix "\"" stripSuffix "\""
        Literal(StringConstant(string))
      }

    override def visitId(ctx: EECParser.IdContext): Tree = {
      import NameOps._
      Ident(ctx.getText.readAs)
    }

    override def visitAlphaId(ctx: EECParser.AlphaIdContext): Tree = {
      import NameOps._
      Ident(ctx.getText.readAs)
    }

    override def visitQualId(ctx: EECParser.QualIdContext): Tree = {
      import scala.language.implicitConversions
      import NameOps._
      def listToRefId(lst: List[String]): Tree = lst match {
        case n :: Nil => Ident(n.readAs)
        case n :: tail => Select(listToRefId(tail), n.readAs)
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
        Select(src, select)
      }
    }

    override def visitType(ctx: EECParser.TypeContext): Tree =
      if ctx.`type` != null then {
        val arg = visitInfixType(ctx.infixType)
        val body = visitType(ctx.`type`)
        FunctionType(arg, body)
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
      val tag = if types.size == 0 then UnitTag else Tuple2Tag
      Apply(Ident(tag), types.toList)
    }

    override def visitPrefixType(ctx: EECParser.PrefixTypeContext): Tree =
      if ctx.simpleType != null then {
        visitSimpleType(ctx.simpleType)
      } else {
        val tag = Ident(ComputationTag)
        val args = visitType(ctx.`type`) :: Nil
        Apply(tag, args)
      }

    override def visitSimpleType(ctx: EECParser.SimpleTypeContext): Tree =
      if ctx.qualId != null then
        visitQualId(ctx.qualId)
      else
        visitType(ctx.`type`)

    // override def visitAscription(ctx: EECParser.AscriptionContext): Tree =
    //   EmptyTree

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
        val Seq(expr, arg) = exprs.map(visitExpr).ensuring(_.size == 2)
        Apply(expr, List(arg))
      }

    override def visitLambda(ctx: EECParser.LambdaContext): Tree = {
      import TreeOps._
      val bindings = visitBindings(ctx.bindings).toList
      val body = visitExpr(ctx.expr)
      Function(bindings, body)
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
      Let(name, value, continuation)
    }

    override def visitCaseExpr(ctx: EECParser.CaseExprContext): Tree = {
      import TreeOps._
      val expr = visitExpr(ctx.expr)
      val cases = visitCases(ctx.cases).toList
      CaseExpr(expr, cases)
    }

    override def visitExpr1(ctx: EECParser.Expr1Context): Tree =
      if ctx.infixExpr != null then {
        visitInfixExpr(ctx.infixExpr)
      } else {
        import scala.language.implicitConversions
        val exprs = ctx.expr
          .ensuring(_.size == 3)
          .map(visitExpr)
        IfThenElse(exprs(0), exprs(1), exprs(2))
      }

    override def visitInfixExpr(ctx: EECParser.InfixExprContext): Tree =
      if ctx.prefixExpr != null then {
        visitPrefixExpr(ctx.prefixExpr)
      } else {
        val id =
          if ctx.OpId != null then {
            import NameOps._
            Ident(ctx.OpId.getText.readAs)
          } else {
            visitAlphaId(ctx.alphaId)
          }
        import scala.language.implicitConversions
        val infixes = ctx.infixExpr
          .ensuring(_.size == 2)
          .map(visitInfixExpr)
        val firstApply = Apply(id, List(infixes(0)))
        Apply(firstApply, List(infixes(1)))
      }

    override def visitPrefixExpr(ctx: EECParser.PrefixExprContext): Tree = {
      val simpleExpr = visitSimpleExpr(ctx.simpleExpr)
      if ctx.Bang != null then {
        val tag = Ident(ComputationTag)
        Apply(tag, List(simpleExpr))
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
      // else {
      //   val simpleExpr = visitSimpleExpr(ctx.simpleExpr)
      //   val argExpr = visitArgumentExpr(ctx.argumentExpr)
      //   Apply(simpleExpr, argExpr)
      // }

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
      CaseClause(pat, guard, body)
    }

    override def visitExprsInParens(
      ctx: EECParser.ExprsInParensContext): Tree = {
        val expr = {
          import scala.language.implicitConversions
          ctx.expr.map(visitExpr)
        }
        if expr.size == 1 then {
          expr(0)
        } else {
          assert(expr.size <= 2)
          if expr.size == 0 then {
            val tag = Ident(UnitTag)
            Apply(tag, Nil)
          } else {
            val tag = Ident(Tuple2Tag)
            Apply(tag, List(expr(0), expr(1)))
          }
        }
      }

    // override def visitArgumentExpr(ctx: EECParser.ArgumentExprContext): ExprTree =
    //   if ctx.expr != null then
    //     visitExpr(ctx.expr)
    //   else
    //     EmptyTree

    override def visitPattern(ctx: EECParser.PatternContext): Tree = {
      val patterns = {
        import scala.language.implicitConversions
        ctx.pattern1.map(visitPattern1)
      }
      if patterns.size == 1 then
        patterns(0)
      else
        Alternative(patterns.toList)
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
          Bind(name, visitPattern3(ctx.pattern3))
        } else {
          Ident(name)
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
          val unitTag = Ident(UnitTag)
          Unapply(unitTag, Nil)
        } else if ctx.Varid != null then {
          import NameOps._
          Ident(ctx.Varid.getText.readAs)
        } else if ctx.literal != null then {
          visitLiteral(ctx.literal)
        } else if ctx.simplePattern != null then { // Bang present
          val simplePat = visitSimplePattern(ctx.simplePattern)
          val computation = Ident(ComputationTag)
          Unapply(computation, List(simplePat))
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
            val tuple2Tag = Ident(Tuple2Tag)
            Unapply(tuple2Tag, patterns.toList)
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
      Tagged(name, typ)
    }

    override def visitDcl(ctx: EECParser.DclContext): Tree =
      visitPrimitiveDcl(ctx.primitiveDcl)

    override def visitPrimitiveDcl(
      ctx: EECParser.PrimitiveDclContext): Tree = {
        import TreeOps._
        import eec.compiler.core.Modifiers._
        visitDefDecl(ctx.defDecl).addModifiers(Set(Modifier.Primitive))
      }

    override def visitDefDecl(ctx: EECParser.DefDeclContext): Tree = {
      val sig = visitDefSig(ctx.defSig)
      val typ = visitType(ctx.`type`)
      Def(Set(), sig, typ, EmptyTree)
    }

    override def visitDef(ctx: EECParser.DefContext): Tree =
      visitDefDef(ctx.defDef)

    override def visitDefDef(ctx: EECParser.DefDefContext): Tree = {
      val defSig = visitDefSig(ctx.defSig)
      val typ = visitType(ctx.`type`)
      val expr = visitExpr(ctx.expr)
      Def(Set(), defSig, typ, expr)
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
        DefSig(varids.head, varids.tail)
      }

    override def visitInfixDefSig(ctx: EECParser.InfixDefSigContext): Tree =
      if ctx.prefixOpSig != null then {
        visitPrefixOpSig(ctx.prefixOpSig)
      } else if ctx.OpId != null then {
        import scala.language.implicitConversions
        import NameOps._
        var args = ctx.Varid
          .ensuring(_.size == 2)
          .map(_.getText.readAs)
          .toList
        DefSig(ctx.OpId.getText.readAs, args)
      } else {
        import scala.language.implicitConversions
        import NameOps._
        var varids = ctx.Varid
          .ensuring(_.size == 3)
          .map(_.getText.readAs)
        DefSig(varids(1), List(varids(0), varids(2)))
      }

    override def visitPrefixOpSig(ctx: EECParser.PrefixOpSigContext): Tree = {
      import NameOps._
      var args = {
        import scala.language.implicitConversions
        ctx.Varid.map(_.getText.readAs).toList
      }
      DefSig(ctx.OpId.getText.readAs, args)
    }

    override def visitPackageInfo(ctx: EECParser.PackageInfoContext): Tree =
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
}