package eec
package compiler
package parsers

object Parsers {

  import TreeParsers._
  import ast.Trees._
  import ast.Trees.Tree._
  import ast.Trees.untyped._
  import core.Names._
  import core.Constants._
  import core.Constants.Constant._
  import error.ParserErrors._
  import org.antlr.v4.runtime.tree.TerminalNode
  import org.antlr.v4.runtime._
  import scala.collection.JavaConversions._

  private[parsers] val eecParser =
    genParser andThen { _.translationUnit }

  private[parsers] val exprParser =
    genParser andThen { _.expr }

  private[parsers] object TreeParsers {

    private[parsers] def (o: String => O) toTreeParser[O](f: O => Tree): (
      String => Tree | ParserError) = {
        import error.ParserErrors.ParserError._
        f compose o andThen recover
      }

    def fromLiteral(ctx: EECParser.LiteralContext): Tree =
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

    def fromId(ctx: EECParser.IdContext): Tree = {
      import NameOps._
      Ident(uTpe, ctx.getText.readAs)
    }

    def fromAlphaId(ctx: EECParser.AlphaIdContext): Tree = {
      import NameOps._
      Ident(uTpe, ctx.getText.readAs)
    }

    def fromQualId(ctx: EECParser.QualIdContext): Tree = {
      import scala.language.implicitConversions
      import NameOps._
      def listToRefId(lst: List[String]): Tree = lst match {
        case n :: Nil => Ident(uTpe, n.readAs)
        case n :: tail => Select(uTpe, listToRefId(tail), n.readAs)
        case Nil => EmptyTree
      }
      listToRefId(ctx.id.reverse.map(_.getText).toList)
    }

    def fromStableId(ctx: EECParser.StableIdContext): Tree = {
      val ids = {
        import scala.language.implicitConversions
        ctx.id.map(fromId).ensuring(_.size <= 2)
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

    def fromType(ctx: EECParser.TypeContext): Tree =
      if ctx.`type` != null then {
        val arg = fromInfixType(ctx.infixType)
        val body = fromType(ctx.`type`)
        Function(uTpe, List(arg), body)
      } else {
        fromInfixType(ctx.infixType)
      }

    def fromInfixType(ctx: EECParser.InfixTypeContext): Tree =
      if ctx.prefixType != null then
        fromPrefixType(ctx.prefixType)
      else
        fromProductType(ctx.productType)

    def fromProductType(ctx: EECParser.ProductTypeContext): Tree = {
      import scala.language.implicitConversions
      val types = ctx.`type`
        .map(fromType)
        .ensuring(l => l.size == 0 || l.size == 2)
      Parens(uTpe, types.toList)
    }

    def fromPrefixType(ctx: EECParser.PrefixTypeContext): Tree =
      if ctx.simpleType != null then {
        fromSimpleType(ctx.simpleType)
      } else {
        val tag = Ident(uTpe, Name.ComputationTag)
        val args = fromType(ctx.`type`) :: Nil
        Apply(uTpe, tag, args)
      }

    def fromSimpleType(ctx: EECParser.SimpleTypeContext): Tree =
      if ctx.qualId != null then
        fromQualId(ctx.qualId)
      else
        fromType(ctx.`type`)

    def fromExpr(ctx: EECParser.ExprContext): Tree =
      if ctx.lambda != null then
        fromLambda(ctx.lambda)
      else if ctx.letExpr != null then
        fromLetExpr(ctx.letExpr)
      else if ctx.caseExpr != null then
        fromCaseExpr(ctx.caseExpr)
      else if ctx.expr1 != null then
        fromExpr1(ctx.expr1)
      else
        fromExprSeqAsApply(ctx.expr)
    
    def fromExprSeqAsApply(
      exprs: java.util.List[EECParser.ExprContext]): Tree = {
        import scala.language.implicitConversions
        import TreeOps._
        val Seq(expr, arg) = exprs.map(fromExpr).ensuring(_.size == 2)
        Apply(uTpe, expr, arg.toList)
      }

    def fromLambda(ctx: EECParser.LambdaContext): Tree = {
      import TreeOps._
      val bindings = fromBindings(ctx.bindings).toList
      val body = fromExpr(ctx.expr)
      Function(uTpe, bindings, body)
    }

    def fromLetExpr(ctx: EECParser.LetExprContext): Tree = {
      import NameOps._
      var name =
        if ctx.Varid != null then
          ctx.Varid.getText.readAs
        else
          ctx.Wildcard.getText.readAs
      var exprs = ctx.expr.ensuring(_.size == 2)
      var value = fromExpr(exprs.get(0))
      var continuation = fromExpr(exprs.get(1))
      Let(uTpe, name, value, continuation)
    }

    def fromCaseExpr(ctx: EECParser.CaseExprContext): Tree = {
      import TreeOps._
      val expr = fromExpr(ctx.expr)
      val cases = fromCases(ctx.cases).toList
      CaseExpr(uTpe, expr, cases)
    }

    def fromExpr1(ctx: EECParser.Expr1Context): Tree =
      if ctx.infixExpr != null then {
        fromInfixExpr(ctx.infixExpr)
      } else {
        import scala.language.implicitConversions
        val exprs = ctx.expr
          .ensuring(_.size == 3)
          .map(fromExpr)
        If(uTpe, exprs(0), exprs(1), exprs(2))
      }

    def fromInfixExpr(ctx: EECParser.InfixExprContext): Tree =
      if ctx.prefixExpr != null then {
        fromPrefixExpr(ctx.prefixExpr)
      } else {
        val id =
          if ctx.OpId != null then {
            import NameOps._
            Ident(uTpe, ctx.OpId.getText.readAs)
          } else {
            fromAlphaId(ctx.alphaId)
          }
        import scala.language.implicitConversions
        val infixes = ctx.infixExpr
          .ensuring(_.size == 2)
          .map(fromInfixExpr)
        val firstApply = Apply(uTpe, id, List(infixes(0)))
        Apply(uTpe, firstApply, List(infixes(1)))
      }

    def fromPrefixExpr(ctx: EECParser.PrefixExprContext): Tree = {
      val simpleExpr = fromSimpleExpr(ctx.simpleExpr)
      if ctx.Bang != null then {
        import NameOps._
        val tag = Ident(uTpe, Name.ComputationTag)
        Apply(uTpe, tag, List(simpleExpr))
      } else {
        simpleExpr
      }
    }

    def fromSimpleExpr(ctx: EECParser.SimpleExprContext): Tree =
      if ctx.literal != null then
        fromLiteral(ctx.literal)
      else if ctx.stableId != null then
        fromStableId(ctx.stableId)
      else
        fromExprsInParens(ctx.exprsInParens)

    def fromCases(ctx: EECParser.CasesContext): Tree = {
      import scala.language.implicitConversions
      val caseClauses = ctx.caseClause.map(fromCaseClause).toList
      TreeSeq(caseClauses)
    }

    def fromCaseClause(ctx: EECParser.CaseClauseContext): Tree = {
      val pat = fromPattern(ctx.pattern)
      val guard =
        if ctx.guard != null then
          fromGuard(ctx.guard)
        else
          EmptyTree
      val body = fromExpr(ctx.expr)
      CaseClause(uTpe, pat, guard, body)
    }

    def fromExprsInParens(
      ctx: EECParser.ExprsInParensContext): Tree = {
        val expr = {
          import scala.language.implicitConversions
          ctx.expr.map(fromExpr)
        }
        if expr.size == 1 then
          expr(0)
        else
          Parens(uTpe, expr.toList)
      }

    def fromPattern(ctx: EECParser.PatternContext): Tree = {
      val patterns = {
        import scala.language.implicitConversions
        ctx.pattern1.map(fromPattern1)
      }
      if patterns.size == 1 then
        patterns(0)
      else
        Alternative(uTpe, patterns.toList)
    }

    def fromPattern1(ctx: EECParser.Pattern1Context): Tree =
      fromPattern2(ctx.pattern2)

    def fromPattern2(ctx: EECParser.Pattern2Context): Tree =
      if ctx.Varid != null then {
        val name = {
          import NameOps._
          ctx.Varid.getText.readAs
        }
        if ctx.pattern3 != null then {
          Bind(uTpe, name, fromPattern3(ctx.pattern3))
        } else {
          Ident(uTpe, name)
        }
      } else {
        fromPattern3(ctx.pattern3)
      }

    def fromPattern3(ctx: EECParser.Pattern3Context): Tree =
      fromSimplePattern(ctx.simplePattern)

    def fromSimplePattern(
      ctx: EECParser.SimplePatternContext): Tree =
        if ctx.Wildcard != null then {
          wildcardIdent
        } else if ctx.getText == "()" then {
          Parens(uTpe, Nil)
        } else if ctx.Varid != null then {
          import NameOps._
          Ident(uTpe, ctx.Varid.getText.readAs)
        } else if ctx.literal != null then {
          fromLiteral(ctx.literal)
        } else if ctx.simplePattern != null then { // Bang present
          val simplePat = fromSimplePattern(ctx.simplePattern)
          val computation = Ident(uTpe, Name.ComputationTag)
          Unapply(uTpe, computation, List(simplePat))
        } else { // tuple
          fromUpToPairPatten(ctx.upToPairPatten)
        }

    def fromUpToPairPatten(
        ctx: EECParser.UpToPairPattenContext): Tree = {
          val patterns = {
            import scala.language.implicitConversions
            ctx.pattern.map(fromPattern)
          }
          if patterns.size == 1 then {
            patterns(0)
          } else {
            assert(patterns.size == 2)
            Parens(uTpe, patterns.toList)
          }
        }

    def fromGuard(ctx: EECParser.GuardContext): Tree =
      fromInfixExpr(ctx.infixExpr)

    def fromBindings(ctx: EECParser.BindingsContext): Tree =
      fromBindingsTagged(ctx.bindingsTagged)

    def fromBindingsTagged(
      ctx: EECParser.BindingsTaggedContext): Tree = {
        import scala.language.implicitConversions
        val bindings = ctx.binding.map(fromBinding).toList
        TreeSeq(bindings)
      }

    def fromBinding(ctx: EECParser.BindingContext): Tree = {
      import TreeOps._
      val List(name) = fromId(ctx.id).toNames
      val typ = fromType(ctx.`type`)
      Tagged(uTpe, name, typ)
    }

    def fromDcl(ctx: EECParser.DclContext): Tree =
      fromPrimitiveDcl(ctx.primitiveDcl)

    def fromPrimitiveDcl(
      ctx: EECParser.PrimitiveDclContext): Tree = {
        import TreeOps._
        import NameOps._
        import eec.compiler.core.Modifiers._
        val modifiers: Set[Modifier] = Set(Modifier.Primitive)
        fromDefDecl(ctx.defDecl).addModifiers(modifiers)
      }

    def fromDefDecl(ctx: EECParser.DefDeclContext): Tree = {
      val sig = fromDefSig(ctx.defSig)
      val typ = fromType(ctx.`type`)
      DefDef(uTpe, Set(), sig, typ, EmptyTree)
    }

    def fromDef(ctx: EECParser.DefContext): Tree =
      fromDefDef(ctx.defDef)

    def fromDefDef(ctx: EECParser.DefDefContext): Tree = {
      val defSig = fromDefSig(ctx.defSig)
      val typ = fromType(ctx.`type`)
      val expr = fromExpr(ctx.expr)
      DefDef(uTpe, Set(), defSig, typ, expr)
    }

    def fromDefSig(ctx: EECParser.DefSigContext): Tree =
      if ctx.infixDefSig != null then {
        fromInfixDefSig(ctx.infixDefSig)
      } else {
        var varids = {
          import scala.language.implicitConversions
          import NameOps._
          ctx.Varid.map(_.getText.readAs).toList
        }
        DefSig(uTpe, varids.head, varids.tail)
      }

    def fromInfixDefSig(ctx: EECParser.InfixDefSigContext): Tree = {
      import scala.language.implicitConversions
      import NameOps._
      if ctx.prefixOpSig != null then {
        fromPrefixOpSig(ctx.prefixOpSig)
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

    def fromPrefixOpSig(ctx: EECParser.PrefixOpSigContext): Tree = {
      import scala.language.implicitConversions
      import NameOps._
      var args = ctx.Varid.map(_.getText.readAs).toList
      DefSig(uTpe, ctx.OpId.getText.readAs, args)
    }

    def fromPackageInfo(ctx: EECParser.PackageInfoContext): Tree =
      fromQualId(ctx.qualId)

    def fromStatSeq(ctx: EECParser.StatSeqContext): Tree = {
      val stats = {
        import scala.language.implicitConversions
        ctx.stat.map(fromStat).toList
      }
      TreeSeq(stats)
    }

    def fromStat(ctx: EECParser.StatContext): Tree =
      if ctx.`def` != null then fromDef(ctx.`def`) else fromDcl(ctx.dcl)

    def fromTranslationUnit(
      ctx: EECParser.TranslationUnitContext): Tree = {
        import TreeOps._
        val pkgId = fromPackageInfo(ctx.packageInfo)
        val stats = fromStatSeq(ctx.statSeq)
        PackageDef(uTpe, pkgId, stats.toList)
      }
  }

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

  private[Parsers] def genParser[C](input: String): EECParser = {
    val charStream = new ANTLRInputStream(input)
    val lexer = new EECLexer(charStream)
    val tokens = new CommonTokenStream(lexer)
    val parser = new EECParser(tokens)
    parser.removeErrorListeners
    parser.addErrorListener(eecErrorListener)
    parser
  }
}