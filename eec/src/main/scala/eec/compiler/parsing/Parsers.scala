package eec
package compiler
package parsing

object Parsers {

  import ast._
  import Trees._
  import Tree._
  import untyped._
  import core.Names._
  import core.Contexts._
  import core.Constants._
  import core.Modifiers._
  import Context._
  import Modifier._
  import Constant._
  import util.{Convert, |>, PostConditions}
  import PostConditions._
  import error.CompilerErrors._
  import org.antlr.v4.runtime._
  import scala.collection.JavaConversions._

  private[parsing] val eecParser =
    genParser `andThen` { _.translationUnit }

  private[parsing] val statParser =
    genParser `andThen` { _.statAsTop }

  private[parsing] val exprParser =
    genParser `andThen` { _.exprAsTop }

  private[this] class ParserSyntaxException(msg: String) extends Exception(msg)

  private[this] inline def defined[A <: AnyRef](a: A): Boolean = a `ne` null

  private[this] val fromList: List[Tree] |> Tree = {
    import implied TreeOps._
    Convert[List[Tree], Tree]
  }

  private[this] val toList: Tree |> List[Tree] = {
    import implied TreeOps._
    Convert[Tree, List[Tree]]
  }

  private[parsing] object TreeParsers {

    def freshId(): Contextual[Id] = ctx.rootCtx.fresh()

    def (o: String => O) toTreeParser[O]
        (f: O => Contextual[Tree])
        (input: String) given Context = {
      import error.CompilerErrors._
      import error.CompilerErrors.CompilerErrorOps._
      f(o(input)).recover {
        case e: ParserSyntaxException =>
          CompilerError.SyntaxError(e.getMessage)
      }
    }

    def fromIntegerLiteral
          (context: EECParser.LiteralContext): Contextual[Tree] = {
      val txt = context.IntegerLiteral.getText
      if txt.endsWith("l") || txt.endsWith("L") then {
        throw new ParserSyntaxException(s"unexpected Long literal `$txt`")
      }
      Literal(BigIntConstant(BigInt(txt)))(freshId(), uTpe)
    }

    def fromFloatingPointLiteral
          (context: EECParser.LiteralContext): Contextual[Tree] = {
      val txt = context.FloatingPointLiteral.getText
      if txt.endsWith("f") || txt.endsWith("F") then {
        throw new ParserSyntaxException(s"unexpected Float literal `$txt`")
      }
      if txt.endsWith("d") || txt.endsWith("D") then {
        throw new ParserSyntaxException(s"unexpected Double literal `$txt`")
      }
      Literal(BigDecConstant(BigDecimal(txt)))(freshId(), uTpe)
    }

    def fromBooleanLiteral
          (context: EECParser.LiteralContext): Contextual[Tree] = {
      val bool = context.BooleanLiteral.getText match {
        case "True" => true
        case _      => false
      }
      Literal(BooleanConstant(bool))(freshId(), uTpe)
    }

    private inline def charText(context: EECParser.LiteralContext): String =
      context.CharacterLiteral
        .getText
        .stripPrefix("'").stripSuffix("'")

    private inline def formatString(text: String): String =
      if text startsWith "\"\"\"" then
        text stripPrefix "\"\"\"" stripSuffix "\"\"\""
      else
        text stripPrefix "\"" stripSuffix "\""

    def fromCharacterLiteral
          (context: EECParser.LiteralContext): Contextual[Tree] = {
      val charStr = charText(context)
      val char =
        if charStr.length == 0 then
          0
        else
          charStr.charAt(0)
      Literal(CharConstant(char))(freshId(), uTpe)
    }

    def fromStringLiteral
          (context: EECParser.LiteralContext): Contextual[Tree] = {
      val string = formatString(context.StringLiteral.getText)
      Literal(StringConstant(string))(freshId(), uTpe)
    }

    def fromLiteral(context: EECParser.LiteralContext): Contextual[Tree] =
      if defined(context.IntegerLiteral) then
        fromIntegerLiteral(context)
      else if defined(context.FloatingPointLiteral) then
        fromFloatingPointLiteral(context)
      else if defined(context.BooleanLiteral) then
        fromBooleanLiteral(context)
      else if defined(context.CharacterLiteral) then
        fromCharacterLiteral(context)
      else
        fromStringLiteral(context)

    def fromId(context: EECParser.IdContext): Contextual[Tree] = {
      val name = {
        import implied NameOps._
        context.getText.readAs
      }
      Ident(name)(freshId(), uTpe)
    }

    def fromAlphaId(context: EECParser.AlphaIdContext): Contextual[Tree] = {
      val name = {
        import implied NameOps._
        context.getText.readAs
      }
      Ident(name)(freshId(), uTpe)
    }

    def fromQualId(context: EECParser.QualIdContext): Contextual[Tree] = {
      import implied NameOps._
      def listToRefId(lst: List[String]): Contextual[Tree] = lst match {
        case n :: Nil   => Ident(n.readAs)(freshId(), uTpe)
        case n :: tail  => Select(listToRefId(tail), n.readAs)(freshId(), uTpe)
        case Nil        => EmptyTree
      }
      val tokens = {
        import scala.language.implicitConversions
        context.id.map(_.getText).toList.reverse
      }
      listToRefId(tokens)
    }

    def fromStableId(context: EECParser.StableIdContext): Contextual[Tree] = {
      val ids = {
        import scala.language.implicitConversions
        context.id.map(fromId).ensuring(result.size <= 2)
      }
      if ids.size == 1 then {
        ids(0)
      } else {
        import TreeOps._
        val src = ids(0)
        val List(select) = ids(1).toNames
        Select(src, select)(freshId(), uTpe)
      }
    }

    def fromType(context: EECParser.TypeContext): Contextual[Tree] =
      if defined(context.func) then
        fromFunc(context.func)
      else
        fromInfixType(context.infixType)

    def fromFunc(context: EECParser.FuncContext): Contextual[Tree] = {
      import scala.language.implicitConversions
      val infixes = context
        .infixType
        .map(fromInfixType)
        .ensuring(result.size >= 2)
        .toList
      val head :: rest = infixes.reverse
      rest.foldLeft(head)((acc, t) => Function(List(t), acc)(freshId(), uTpe))
    }

    def fromInfixType(context: EECParser.InfixTypeContext): Contextual[Tree] =
      if defined(context.simpleType) then
        fromSimpleType(context.simpleType)
      else
        fromPrefixType(context.prefixType)

    def fromProductType
          (context: EECParser.ProductTypeContext): Contextual[Tree] = {
      import scala.language.implicitConversions
      val types = context.`type`.map(fromType)
      if types.size == 1 then
        types(0)
      else
        Parens(types.toList)(freshId(), uTpe)
    }

    def fromPrefixType
          (context: EECParser.PrefixTypeContext): Contextual[Tree] = {
      val simpleType = fromSimpleType(context.simpleType)
      val tag   = Ident(Name.ComputationTag)(freshId(), uTpe)
      val args  = simpleType :: Nil
      Apply(tag, args)(freshId(), uTpe)
    }

    def fromSimpleType(context: EECParser.SimpleTypeContext): Contextual[Tree] =
      if defined(context.qualId) then
        fromQualId(context.qualId)
      else
        fromProductType(context.productType)

    def fromExpr(context: EECParser.ExprContext): Contextual[Tree] =
      if defined(context.lambda) then
        fromLambda(context.lambda)
      else if defined(context.letExpr) then
        fromLetExpr(context.letExpr)
      else if defined(context.caseExpr) then
        fromCaseExpr(context.caseExpr)
      else if defined(context.expr1) then
        fromExpr1(context.expr1)
      else
        fromExprSeqAsApply(context.expr)
    
    def fromExprSeqAsApply
          (exprs: java.util.List[EECParser.ExprContext]): Contextual[Tree] = {
      import scala.language.implicitConversions
      val Seq(expr, arg) = exprs.map(fromExpr).ensuring(result.size == 2)
      val args = toList(arg)
      Apply(expr, args)(freshId(), uTpe)
    }

    def fromLambda(context: EECParser.LambdaContext): Contextual[Tree] = {
      val bindings  = toList(fromBindings(context.bindings))
      val body      = fromExpr(context.expr)
      Function(bindings, body)(freshId(), uTpe)
    }

    def fromLetExpr(context: EECParser.LetExprContext): Contextual[Tree] = {
      import implied NameOps._
      var name =
        if defined(context.Varid) then
          context.Varid.getText.readAs
        else
          context.Wildcard.getText.readAs
      var exprs         = context.expr.ensuring(result.size == 2)
      var value         = fromExpr(exprs.get(0))
      var continuation  = fromExpr(exprs.get(1))
      val ident         = Ident(name)(freshId(), uTpe)
      Let(ident, value, continuation)(freshId(), uTpe)
    }

    def fromCaseExpr(context: EECParser.CaseExprContext): Contextual[Tree] = {
      val selector = fromExpr(context.expr)
      val cases = toList(fromCases(context.cases))
      CaseExpr(selector, cases)(freshId(), uTpe)
    }

    def fromIfElse(context: EECParser.Expr1Context) : Contextual[Tree] = {
      import scala.language.implicitConversions
      import types.Types.TypeOps._
      val exprs = context.expr
        .ensuring(result.size == 3)
        .map(fromExpr)
      val patTrue   = Literal(BooleanConstant(true))(freshId(), uTpe)
      val patFalse  = Literal(BooleanConstant(false))(freshId(), uTpe)
      val caseTrue  = CaseClause(patTrue, EmptyTree, exprs(1))(freshId(), uTpe)
      val caseFalse = CaseClause(patFalse, EmptyTree, exprs(2))(freshId(), uTpe)
      val selector  = exprs(0).withType(uTpeBoolean)
      CaseExpr(selector, List(caseTrue, caseFalse))(freshId(), uTpe)
    }

    def fromExpr1(context: EECParser.Expr1Context): Contextual[Tree] =
      if defined(context.infixExpr) then
        fromInfixExpr(context.infixExpr)
      else
        fromIfElse(context)

    def fromInfixApplication
          (context: EECParser.InfixExprContext): Contextual[Tree] = {
      val id =
        if defined(context.OpId) then {
          import implied NameOps._
          Ident(context.OpId.getText.readAs)(freshId(), uTpe)
        } else {
          fromAlphaId(context.alphaId)
        }
      import scala.language.implicitConversions
      val infixes = context.infixExpr
        .ensuring(result.size == 2)
        .map(fromInfixExpr)
      val firstApply = Apply(id, List(infixes(0)))(freshId(), uTpe)
      Apply(firstApply, List(infixes(1)))(freshId(), uTpe)
    }

    def fromInfixExpr(context: EECParser.InfixExprContext): Contextual[Tree] =
      if defined(context.prefixExpr) then
        fromPrefixExpr(context.prefixExpr)
      else
        fromInfixApplication(context)

    def wrapComputation(tree: Tree): Contextual[Tree] = {
      import Name._
      val tag = Ident(ComputationTag)(freshId(), uTpe)
      Apply(tag, List(tree))(freshId(), uTpe)
    }

    def fromPrefixExpr
          (context: EECParser.PrefixExprContext): Contextual[Tree] = {
      val simpleExpr = fromSimpleExpr(context.simpleExpr)
      if defined(context.Bang) then
        wrapComputation(simpleExpr)
      else
        simpleExpr
    }

    def fromSimpleExpr(context: EECParser.SimpleExprContext): Contextual[Tree] =
      if defined(context.literal) then
        fromLiteral(context.literal)
      else if defined(context.stableId) then
        fromStableId(context.stableId)
      else
        fromExprsInParens(context.exprsInParens)

    def fromCases(context: EECParser.CasesContext): Contextual[Tree] = {
      val caseClauses = {
        import scala.language.implicitConversions
        context.caseClause.map(fromCaseClause).toList
      }
      fromList(caseClauses)
    }

    def fromCaseClause
          (context: EECParser.CaseClauseContext): Contextual[Tree] = {
      val pat = fromPattern(context.pattern)
      val guard =
        if defined(context.guard) then
          fromGuard(context.guard)
        else
          EmptyTree
      val body = fromExpr(context.expr)
      CaseClause(pat, guard, body)(freshId(), uTpe)
    }

    def fromExprsInParens
          (context: EECParser.ExprsInParensContext): Contextual[Tree] = {
      val expr = {
        import scala.language.implicitConversions
        context.expr.map(fromExpr)
      }
      if expr.size == 1 then
        expr(0)
      else
        Parens(expr.toList)(freshId(), uTpe)
    }

    def fromPattern(context: EECParser.PatternContext): Contextual[Tree] = {
      val patterns = {
        import scala.language.implicitConversions
        context.pattern1.map(fromPattern1)
      }
      if patterns.size == 1 then
        patterns(0)
      else
        Alternative(patterns.toList)(freshId(), uTpe)
    }

    def fromPattern1(context: EECParser.Pattern1Context): Contextual[Tree] =
      fromPattern2(context.pattern2)

    def fromBind(context: EECParser.Pattern2Context): Contextual[Tree] = {
      val name = {
        import implied NameOps._
        context.Varid.getText.readAs
      }
      if defined(context.pattern3) then
        Bind(name, fromPattern3(context.pattern3))(freshId(), uTpe)
      else
        Ident(name)(freshId(), uTpe)
    }

    def fromPattern2(context: EECParser.Pattern2Context): Contextual[Tree] =
      if defined(context.Varid) then
        fromBind(context)
      else
        fromPattern3(context.pattern3)

    def fromPattern3(context: EECParser.Pattern3Context): Contextual[Tree] =
      fromSimplePattern(context.simplePattern)

    inline def fromVaridPattern
                (context: EECParser.SimplePatternContext): Contextual[Tree] = {
      val name = {
        import implied NameOps._
        context.Varid.getText.readAs
      }
      Ident(name)(freshId(), uTpe)
    }

    inline def fromUnitPattern: Contextual[Tree] =
      Parens(Nil)(freshId(), uTpe)

    def fromSimplePattern
          (context: EECParser.SimplePatternContext): Contextual[Tree] =
      if defined(context.Wildcard) then
        wildcardIdent
      else if context.getText == "()" then
        fromUnitPattern
      else if defined(context.Varid) then
        fromVaridPattern(context)
      else if defined(context.literal) then
        fromLiteral(context.literal)
      else
        fromPatterns(context.patterns)

    def fromPatterns
          (context: EECParser.PatternsContext): Contextual[Tree] = {
      val patterns = {
        import scala.language.implicitConversions
        context.pattern.map(fromPattern)
      }
      if patterns.size == 1 then
        patterns(0)
      else
        Parens(patterns.toList)(freshId(), uTpe)
    }

    def fromGuard(context: EECParser.GuardContext): Contextual[Tree] =
      fromInfixExpr(context.infixExpr)

    def fromBindings(context: EECParser.BindingsContext): Contextual[Tree] =
      fromBindingsTagged(context.bindingsTagged)

    def fromBindingsTagged
          (context: EECParser.BindingsTaggedContext): Contextual[Tree] = {
      val bindings = {
        import scala.language.implicitConversions
        context.binding.map(fromBinding).toList
      }
      fromList(bindings)
    }

    def fromBinding(context: EECParser.BindingContext): Contextual[Tree] = {
      import TreeOps._
      val List(name) = fromId(context.id).toNames
      val typ = fromType(context.`type`)
      Tagged(name, typ)(freshId(), uTpe)
    }

    def fromDcl(context: EECParser.DclContext): Contextual[Tree] =
      fromPrimitiveDcl(context.primitiveDcl)

    def fromPrimitiveDcl
          (context: EECParser.PrimitiveDclContext): Contextual[Tree] = {
      import TreeOps._
      val modifiers = Set(Primitive)
      fromDefDecl(context.defDecl).addModifiers(modifiers)
    }

    def fromDefDecl(context: EECParser.DefDeclContext): Contextual[Tree] = {
      val sig = fromDefSig(context.defSig)
      val typ = fromType(context.`type`)
      DefDef(Set(), sig, typ, EmptyTree)(freshId(), uTpe)
    }

    def fromDef(context: EECParser.DefContext): Contextual[Tree] =
      fromDefDef(context.defDef)

    def fromDefDef(context: EECParser.DefDefContext): Contextual[Tree] = {
      val defSig  = fromDefSig(context.defSig)
      val typ     = fromType(context.`type`)
      val expr    = fromExpr(context.expr)
      DefDef(Set(), defSig, typ, expr)(freshId(), uTpe)
    }

    def fromDefSig(context: EECParser.DefSigContext): Contextual[Tree] =
      if defined(context.infixDefSig) then
        fromInfixDefSig(context.infixDefSig)
      else
        fromPrefixDefSig(context)

    def fromPrefixDefSig
          (context: EECParser.DefSigContext): Contextual[Tree] = {
      var name = {
        import implied TreeOps._
        Convert[Tree, Name](fromAlphaId(context.alphaId))
      }
      var varids = {
        import scala.language.implicitConversions
        import implied NameOps._
        context.Varid.map(_.getText.readAs).toList
      }
      val identArgs = varids.map(n => Ident(n)(freshId(), uTpe))
      DefSig(name, identArgs)(freshId(), uTpe)
    }

    def fromInfixDefSig
          (context: EECParser.InfixDefSigContext): Contextual[Tree] =
      if defined(context.prefixOpSig) then
        fromPrefixOpSig(context.prefixOpSig)
      else if defined(context.OpId) then
        fromInfixOpSig(context)
      else
        fromInfixAlphaSig(context)

    def infixArgs
        (context: EECParser.InfixDefSigContext): Contextual[List[Tree]] = {
      import scala.language.implicitConversions
      import implied NameOps._
      context.Varid
        .ensuring(result.size == 2)
        .map(_.getText.readAs)
        .map(Ident(_)(freshId(), uTpe))
        .toList
    }

    def fromInfixOpSig
          (context: EECParser.InfixDefSigContext): Contextual[Tree] = {
      var args = infixArgs(context)
      var name = {
        import implied NameOps._
        context.OpId.getText.readAs
      }
      DefSig(name, args)(freshId(), uTpe)
    }

    def fromInfixAlphaSig
          (context: EECParser.InfixDefSigContext): Contextual[Tree] = {
      var args = infixArgs(context)
      val name = {
        import implied TreeOps._
        Convert[Tree, Name](fromAlphaId(context.alphaId))
      }
      DefSig(name, args)(freshId(), uTpe)
    }

    def fromPrefixOpSig
          (context: EECParser.PrefixOpSigContext): Contextual[Tree] = {
      import scala.language.implicitConversions
      import implied NameOps._
      var args = context.Varid
        .map { _.getText.readAs }
        .map { Ident(_)(freshId(), uTpe) }
        .toList
      val name = context.OpId.getText.readAs
      DefSig(name, args)(freshId(), uTpe)
    }

    def fromPackageInfo
          (context: EECParser.PackageInfoContext): Contextual[Tree] =
      fromQualId(context.qualId)

    def fromStatSeq(context: EECParser.StatSeqContext): Contextual[Tree] = {
      import scala.language.implicitConversions
      import implied TreeOps._
      val stats = context.stat.map(fromStat).toList
      fromList(stats)
    }

    def fromStat(context: EECParser.StatContext): Contextual[Tree] =
      if defined(context.`def`) then
        fromDef(context.`def`)
      else
        fromDcl(context.dcl)

    def fromStatAsTop(context: EECParser.StatAsTopContext): Contextual[Tree] =
      fromStat(context.stat)

    def fromExprAsTop(context: EECParser.ExprAsTopContext): Contextual[Tree] =
      fromExpr(context.expr)

    def fromTranslationUnit
          (context: EECParser.TranslationUnitContext): Contextual[Tree] = {
      val pkgId = fromPackageInfo(context.packageInfo)
      val stats =
        if defined(context.statSeq) then
          fromStatSeq(context.statSeq)
        else
          EmptyTree
      PackageDef(pkgId, toList(stats))(freshId(), uTpe)
    }
  }

  private[Parsers] val eecErrorListener: BaseErrorListener = new {
    override def syntaxError
          (recognizer: Recognizer[_, _],
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