package eec
package compiler
package parsing

object Parsers {

  import TreeParsers._
  import ast._
  import Trees._
  import Tree._
  import untyped._
  import core.Names._
  import core.Contexts._
  import Context._
  import core.Constants._
  import core.Constants.Constant._
  import error.CompilerErrors._
  import org.antlr.v4.runtime.tree.TerminalNode
  import org.antlr.v4.runtime._

  import scala.collection.JavaConversions._

  private[parsing] val eecParser =
    genParser andThen { _.translationUnit }

  private[parsing] val exprParser =
    genParser andThen { _.expr }

  private[parsing] val typeParser =
    genParser andThen { _.`type` }

  private[Parsers] class ParserSyntaxException(msg: String) extends Exception(msg)

  private[parsing] object TreeParsers {

    def freshId: Contextual[Id] = ctx.rootCtx.fresh

    def (o: String => O) toTreeParser[O](f: O => Contextual[Tree]): (
      String => Contextual[Checked[Tree]]) = {
        import error.CompilerErrors._
        import error.CompilerErrors.CompilerErrorOps._
        input => f(o(input)).recover {
          case e: ParserSyntaxException =>
            CompilerError.SyntaxError(e.getMessage)
        }
      }

    def fromLiteral(context: EECParser.LiteralContext): Contextual[Tree] =
      if context.IntegerLiteral ne null then {
        val txt = context.IntegerLiteral.getText
        if txt.endsWith("l") || txt.endsWith("L") then {
          throw new ParserSyntaxException(s"unexpected Long literal `$txt`")
        }
        Literal(BigIntConstant(BigInt(txt)))(freshId, uTpe)
      } else if context.FloatingPointLiteral ne null then {
        val txt = context.FloatingPointLiteral.getText
        if txt.endsWith("f") || txt.endsWith("F") then {
          throw new ParserSyntaxException(s"unexpected Float literal `$txt`")
        }
        if txt.endsWith("d") || txt.endsWith("D") then {
          throw new ParserSyntaxException(s"unexpected Double literal `$txt`")
        }
        Literal(BigDecConstant(BigDecimal(txt)))(freshId, uTpe)
      } else if context.BooleanLiteral ne null then {
        val bool = context.BooleanLiteral.getText match {
          case "True" => true
          case _ => false
        }
        Literal(BooleanConstant(bool))(freshId, uTpe)
      } else if context.CharacterLiteral ne null then {
        val charStr = context.CharacterLiteral.getText
          .stripPrefix("'").stripSuffix("'")
        val char = if charStr.length == 0 then 0 else charStr.charAt(0)
        Literal(CharConstant(char))(freshId, uTpe)
      } else { // StringLiteral
        val text = context.StringLiteral.getText
        val string =
          if text startsWith "\"\"\"" then
            text stripPrefix "\"\"\"" stripSuffix "\"\"\""
          else
            text stripPrefix "\"" stripSuffix "\""
        Literal(StringConstant(string))(freshId, uTpe)
      }

    def fromId(context: EECParser.IdContext): Contextual[Tree] = {
      import implied NameOps._
      Ident(context.getText.readAs)(freshId, uTpe)
    }

    def fromAlphaId(context: EECParser.AlphaIdContext): Contextual[Tree] = {
      import implied NameOps._
      Ident(context.getText.readAs)(freshId, uTpe)
    }

    def fromQualId(context: EECParser.QualIdContext): Contextual[Tree] = {
      import scala.language.implicitConversions
      import implied NameOps._
      def listToRefId(lst: List[String]): Contextual[Tree] = lst match {
        case n :: Nil => Ident(n.readAs)(freshId, uTpe)
        case n :: tail => Select(listToRefId(tail), n.readAs)(freshId, uTpe)
        case Nil => EmptyTree
      }
      listToRefId(context.id.reverse.map(_.getText).toList)
    }

    def fromStableId(context: EECParser.StableIdContext): Contextual[Tree] = {
      val ids = {
        import scala.language.implicitConversions
        context.id.map(fromId).ensuring(_.size <= 2)
      }
      if ids.size == 1 then {
        ids(0)
      } else {
        import TreeOps._
        val src = ids(0)
        val List(select) = ids(1).toNames
        Select(src, select)(freshId, uTpe)
      }
    }

    def fromType(context: EECParser.TypeContext): Contextual[Tree] =
      if context.`type` ne null then {
        val arg = fromInfixType(context.infixType)
        val body = fromType(context.`type`)
        Function(List(arg), body)(freshId, uTpe)
      } else {
        fromInfixType(context.infixType)
      }

    def fromInfixType(context: EECParser.InfixTypeContext): Contextual[Tree] =
      if context.prefixType ne null then
        fromPrefixType(context.prefixType)
      else
        fromProductType(context.productType)

    def fromProductType(context: EECParser.ProductTypeContext): Contextual[Tree] = {
      import scala.language.implicitConversions
      val types = context.`type`
        .map(fromType)
        .ensuring(l => l.size == 0 || l.size == 2)
      Parens(types.toList)(freshId, uTpe)
    }

    def fromPrefixType(context: EECParser.PrefixTypeContext): Contextual[Tree] =
      if context.simpleType ne null then {
        fromSimpleType(context.simpleType)
      } else {
        val tag = Ident(Name.ComputationTag)(freshId, uTpe)
        val args = fromType(context.`type`) :: Nil
        Apply(tag, args)(freshId, uTpe)
      }

    def fromSimpleType(context: EECParser.SimpleTypeContext): Contextual[Tree] =
      if context.qualId ne null then
        fromQualId(context.qualId)
      else
        fromType(context.`type`)

    def fromExpr(context: EECParser.ExprContext): Contextual[Tree] =
      if context.lambda ne null then
        fromLambda(context.lambda)
      else if context.letExpr ne null then
        fromLetExpr(context.letExpr)
      else if context.caseExpr ne null then
        fromCaseExpr(context.caseExpr)
      else if context.expr1 ne null then
        fromExpr1(context.expr1)
      else
        fromExprSeqAsApply(context.expr)
    
    def fromExprSeqAsApply(
      exprs: java.util.List[EECParser.ExprContext]): Contextual[Tree] = {
        import scala.language.implicitConversions
        import TreeOps._
        val Seq(expr, arg) = exprs.map(fromExpr).ensuring(_.size == 2)
        Apply(expr, arg.toList)(freshId, uTpe)
      }

    def fromLambda(context: EECParser.LambdaContext): Contextual[Tree] = {
      import TreeOps._
      val bindings = fromBindings(context.bindings).toList
      val body = fromExpr(context.expr)
      Function(bindings, body)(freshId, uTpe)
    }

    def fromLetExpr(context: EECParser.LetExprContext): Contextual[Tree] = {
      import implied NameOps._
      var name =
        if context.Varid ne null then
          context.Varid.getText.readAs
        else
          context.Wildcard.getText.readAs
      var exprs = context.expr.ensuring(_.size == 2)
      var value = fromExpr(exprs.get(0))
      var continuation = fromExpr(exprs.get(1))
      val ident = Ident(name)(freshId, uTpe)
      Let(ident, value, continuation)(freshId, uTpe)
    }

    def fromCaseExpr(context: EECParser.CaseExprContext): Contextual[Tree] = {
      import TreeOps._
      val selector = fromExpr(context.expr)
      val cases = fromCases(context.cases).toList
      CaseExpr(selector, cases)(freshId, uTpe)
    }

    def fromExpr1(context: EECParser.Expr1Context): Contextual[Tree] =
      if context.infixExpr ne null then {
        fromInfixExpr(context.infixExpr)
      } else {
        import scala.language.implicitConversions
        val exprs = context.expr
          .ensuring(_.size == 3)
          .map(fromExpr)
        val patTrue   = Literal(BooleanConstant(true))(freshId, uTpe)
        val patFalse  = Literal(BooleanConstant(false))(freshId, uTpe)
        val caseTrue  = CaseClause(patTrue, EmptyTree, exprs(1))(freshId, uTpe)
        val caseFalse = CaseClause(patFalse, EmptyTree, exprs(2))(freshId, uTpe)
        CaseExpr(exprs(0), List(caseTrue, caseFalse))(freshId, uTpe)
      }

    def fromInfixExpr(context: EECParser.InfixExprContext): Contextual[Tree] =
      if context.prefixExpr ne null then {
        fromPrefixExpr(context.prefixExpr)
      } else {
        val id =
          if context.OpId ne null then {
            import implied NameOps._
            Ident(context.OpId.getText.readAs)(freshId, uTpe)
          } else {
            fromAlphaId(context.alphaId)
          }
        import scala.language.implicitConversions
        val infixes = context.infixExpr
          .ensuring(_.size == 2)
          .map(fromInfixExpr)
        val firstApply = Apply(id, List(infixes(0)))(freshId, uTpe)
        Apply(firstApply, List(infixes(1)))(freshId, uTpe)
      }

    def fromPrefixExpr(context: EECParser.PrefixExprContext): Contextual[Tree] = {
      val simpleExpr = fromSimpleExpr(context.simpleExpr)
      if context.Bang ne null then {
        import NameOps._
        val tag = Ident(Name.ComputationTag)(freshId, uTpe)
        Apply(tag, List(simpleExpr))(freshId, uTpe)
      } else {
        simpleExpr
      }
    }

    def fromSimpleExpr(context: EECParser.SimpleExprContext): Contextual[Tree] =
      if context.literal ne null then
        fromLiteral(context.literal)
      else if context.stableId ne null then
        fromStableId(context.stableId)
      else
        fromExprsInParens(context.exprsInParens)

    def fromCases(context: EECParser.CasesContext): Contextual[Tree] = {
      import scala.language.implicitConversions
      import TreeOps._
      val caseClauses = context.caseClause.map(fromCaseClause).toList
      caseClauses.toTree
    }

    def fromCaseClause(context: EECParser.CaseClauseContext): Contextual[Tree] = {
      val pat = fromPattern(context.pattern)
      val guard =
        if context.guard ne null then
          fromGuard(context.guard)
        else
          EmptyTree
      val body = fromExpr(context.expr)
      CaseClause(pat, guard, body)(freshId, uTpe)
    }

    def fromExprsInParens(
      context: EECParser.ExprsInParensContext): Contextual[Tree] = {
        val expr = {
          import scala.language.implicitConversions
          context.expr.map(fromExpr)
        }
        if expr.size == 1 then
          expr(0)
        else
          Parens(expr.toList)(freshId, uTpe)
      }

    def fromPattern(context: EECParser.PatternContext): Contextual[Tree] = {
      val patterns = {
        import scala.language.implicitConversions
        context.pattern1.map(fromPattern1)
      }
      if patterns.size == 1 then
        patterns(0)
      else
        Alternative(patterns.toList)(freshId, uTpe)
    }

    def fromPattern1(context: EECParser.Pattern1Context): Contextual[Tree] =
      fromPattern2(context.pattern2)

    def fromPattern2(context: EECParser.Pattern2Context): Contextual[Tree] = {

      def fromVarid: Tree = {
        import implied NameOps._
        val name = context.Varid.getText.readAs
        if context.pattern3 ne null then
          Bind(name, fromPattern3(context.pattern3))(freshId, uTpe)
        else
          Ident(name)(freshId, uTpe)
      }

      if context.Varid ne null then
        fromVarid
      else
        fromPattern3(context.pattern3)
    }

    def fromPattern3(context: EECParser.Pattern3Context): Contextual[Tree] =
      fromSimplePattern(context.simplePattern)

    def fromSimplePattern(
      context: EECParser.SimplePatternContext): Contextual[Tree] =
        if context.Wildcard ne null then {
          wildcardIdent
        } else if context.getText == "()" then {
          Parens(Nil)(freshId, uTpe)
        } else if context.Varid ne null then {
          import implied NameOps._
          Ident(context.Varid.getText.readAs)(freshId, uTpe)
        } else if context.literal ne null then {
          fromLiteral(context.literal)
        } else if context.simplePattern ne null then { // Bang present
          val simplePat = fromSimplePattern(context.simplePattern)
          val computation = Ident(Name.ComputationTag)(freshId, uTpe)
          Unapply(computation, List(simplePat))(freshId, uTpe)
        } else { // tuple
          fromUpToPairPatten(context.upToPairPatten)
        }

    def fromUpToPairPatten(
        context: EECParser.UpToPairPattenContext): Contextual[Tree] = {
          val patterns = {
            import scala.language.implicitConversions
            context.pattern.map(fromPattern)
          }
          if patterns.size == 1 then {
            patterns(0)
          } else {
            assert(patterns.size == 2)
            Parens(patterns.toList)(freshId, uTpe)
          }
        }

    def fromGuard(context: EECParser.GuardContext): Contextual[Tree] =
      fromInfixExpr(context.infixExpr)

    def fromBindings(context: EECParser.BindingsContext): Contextual[Tree] =
      fromBindingsTagged(context.bindingsTagged)

    def fromBindingsTagged(
      context: EECParser.BindingsTaggedContext): Contextual[Tree] = {
        import scala.language.implicitConversions
        import TreeOps._
        context.binding.map(fromBinding).toList.toTree
      }

    def fromBinding(context: EECParser.BindingContext): Contextual[Tree] = {
      import TreeOps._
      val List(name) = fromId(context.id).toNames
      val typ = fromType(context.`type`)
      Tagged(name, typ)(freshId, uTpe)
    }

    def fromDcl(context: EECParser.DclContext): Contextual[Tree] =
      fromPrimitiveDcl(context.primitiveDcl)

    def fromPrimitiveDcl(
      context: EECParser.PrimitiveDclContext): Contextual[Tree] = {
        import TreeOps._
        import NameOps._
        import eec.compiler.core.Modifiers._
        val modifiers: Set[Modifier] = Set(Modifier.Primitive)
        fromDefDecl(context.defDecl).addModifiers(modifiers)
      }

    def fromDefDecl(context: EECParser.DefDeclContext): Contextual[Tree] = {
      val sig = fromDefSig(context.defSig)
      val typ = fromType(context.`type`)
      DefDef(Set(), sig, typ, EmptyTree)(freshId, uTpe)
    }

    def fromDef(context: EECParser.DefContext): Contextual[Tree] =
      fromDefDef(context.defDef)

    def fromDefDef(context: EECParser.DefDefContext): Contextual[Tree] = {
      val defSig = fromDefSig(context.defSig)
      val typ = fromType(context.`type`)
      val expr = fromExpr(context.expr)
      DefDef(Set(), defSig, typ, expr)(freshId, uTpe)
    }

    def fromDefSig(context: EECParser.DefSigContext): Contextual[Tree] =
      if context.infixDefSig ne null then {
        fromInfixDefSig(context.infixDefSig)
      } else {
        var varids = {
          import scala.language.implicitConversions
          import implied NameOps._
          context.Varid.map(_.getText.readAs).toList
        }
        val identArgs = varids.tail.map(n => Ident(n)(freshId, uTpe))
        DefSig(varids.head, identArgs)(freshId, uTpe)
      }

    def fromInfixDefSig(context: EECParser.InfixDefSigContext): Contextual[Tree] = {
      import scala.language.implicitConversions
      import implied NameOps._
      if context.prefixOpSig ne null then {
        fromPrefixOpSig(context.prefixOpSig)
      } else if context.OpId ne null then {
        var args = context.Varid
          .ensuring(_.size == 2)
          .map(_.getText.readAs)
          .map(n => Ident(n)(freshId, uTpe))
          .toList
        DefSig(context.OpId.getText.readAs, args)(freshId, uTpe)
      } else {
        var varids = context.Varid
          .ensuring(_.size == 3)
          .map(_.getText.readAs)
        val name = varids(1)
        val args = List(varids(0), varids(1)).map(n => Ident(n)(freshId, uTpe))
        DefSig(name, args)(freshId, uTpe)
      }
    }

    def fromPrefixOpSig(context: EECParser.PrefixOpSigContext): Contextual[Tree] = {
      import scala.language.implicitConversions
      import implied NameOps._
      var args = context.Varid.map(_.getText.readAs)
        .map(n => Ident(n)(freshId, uTpe))
        .toList
      DefSig(context.OpId.getText.readAs, args)(freshId, uTpe)
    }

    def fromPackageInfo(context: EECParser.PackageInfoContext): Contextual[Tree] =
      fromQualId(context.qualId)

    def fromStatSeq(context: EECParser.StatSeqContext): Contextual[Tree] = {
      import scala.language.implicitConversions
      import TreeOps._
      context.stat.map(fromStat).toList.toTree
    }

    def fromStat(context: EECParser.StatContext): Contextual[Tree] =
      if context.`def` ne null then fromDef(context.`def`) else fromDcl(context.dcl)

    def fromTranslationUnit(
      context: EECParser.TranslationUnitContext): Contextual[Tree] = {
        import TreeOps._
        val pkgId = fromPackageInfo(context.packageInfo)
        val stats =
          if context.statSeq ne null then
            fromStatSeq(context.statSeq)
          else
            EmptyTree
        PackageDef(pkgId, stats.toList)(freshId, uTpe)
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
    val charStream  = new ANTLRInputStream(input)
    val lexer       = new EECLexer(charStream)
    val tokens      = new CommonTokenStream(lexer)
    val parser      = new EECParser(tokens)
    lexer.removeErrorListeners
    lexer.addErrorListener(eecErrorListener)
    parser.removeErrorListeners
    parser.addErrorListener(eecErrorListener)
    parser
  }
}