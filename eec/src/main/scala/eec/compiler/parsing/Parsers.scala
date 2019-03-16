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
  import IdGen._
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

  private[parsing] def fromStatAsTop
      (context: EECParser.StatAsTopContext) given IdGen: Tree =
    fromStat(context.stat)

  private[parsing] def fromExprAsTop
      (context: EECParser.ExprAsTopContext) given IdGen: Tree =
    fromExpr(context.expr)

  private[parsing] def fromTranslationUnit
      (context: EECParser.TranslationUnitContext) given IdGen: Tree = {
    val pkgId = fromPackageInfo(context.packageInfo)
    val stats =
      if defined(context.statSeq) then
        fromStatSeq(context.statSeq)
      else
        EmptyTree
    PackageDef(pkgId, toList(stats))(freshId(), uTpe)
  }

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

  private[this] def freshId() given IdGen: Id = idGen.fresh()

  def (o: String => O) toTreeParser[O]
      (f: O => IdMaker[Tree])
      (input: String) given IdGen = {
    import error.CompilerErrors._
    import error.CompilerErrors.CompilerErrorOps._
    f(o(input)).recover {
      case e: ParserSyntaxException =>
        CompilerError.SyntaxError(e.getMessage)
    }
  }

  private[this] def fromIntegerLiteral
      (context: EECParser.LiteralContext) given IdGen: Tree = {
    val txt = context.IntegerLiteral.getText
    if txt.endsWith("l") || txt.endsWith("L") then {
      throw new ParserSyntaxException(s"unexpected Long literal `$txt`")
    }
    Literal(BigIntConstant(BigInt(txt)))(freshId(), uTpe)
  }

  private[this] def fromFloatingPointLiteral
      (context: EECParser.LiteralContext) given IdGen: Tree = {
    val txt = context.FloatingPointLiteral.getText
    if txt.endsWith("f") || txt.endsWith("F") then {
      throw new ParserSyntaxException(s"unexpected Float literal `$txt`")
    }
    if txt.endsWith("d") || txt.endsWith("D") then {
      throw new ParserSyntaxException(s"unexpected Double literal `$txt`")
    }
    Literal(BigDecConstant(BigDecimal(txt)))(freshId(), uTpe)
  }

  private[this] def fromBooleanLiteral
      (context: EECParser.LiteralContext) given IdGen: Tree = {
    val bool = context.BooleanLiteral.getText match {
      case "True" => true
      case _      => false
    }
    Literal(BooleanConstant(bool))(freshId(), uTpe)
  }

  private[this] def charText(context: EECParser.LiteralContext): String =
    context.CharacterLiteral
      .getText
      .stripPrefix("'").stripSuffix("'")

  private[this] def formatString(text: String): String =
    if text startsWith "\"\"\"" then
      text stripPrefix "\"\"\"" stripSuffix "\"\"\""
    else
      text stripPrefix "\"" stripSuffix "\""

  private[this] def fromCharacterLiteral
      (context: EECParser.LiteralContext) given IdGen: Tree = {
    val charStr = charText(context)
    val char =
      if charStr.length == 0 then
        0
      else
        charStr.charAt(0)
    Literal(CharConstant(char))(freshId(), uTpe)
  }

  private[this] def fromStringLiteral
      (context: EECParser.LiteralContext) given IdGen: Tree = {
    val string = formatString(context.StringLiteral.getText)
    Literal(StringConstant(string))(freshId(), uTpe)
  }

  private[this] def fromLiteral
      (context: EECParser.LiteralContext) given IdGen: Tree =
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

  private[this] def fromId
      (context: EECParser.IdContext) given IdGen: Tree = {
    val name = {
      import implied NameOps._
      context.getText.readAs
    }
    Ident(name)(freshId(), uTpe)
  }

  private[this] def fromAlphaId
      (context: EECParser.AlphaIdContext) given IdGen: Tree = {
    val name = {
      import implied NameOps._
      context.getText.readAs
    }
    Ident(name)(freshId(), uTpe)
  }

  private[this] def namesToTree(lst: List[Name]) given IdGen: Tree = lst match {
    case n :: Nil   => Ident(n)(freshId(), uTpe)
    case n :: tail  => Select(namesToTree(tail), n)(freshId(), uTpe)
    case Nil        => EmptyTree
  }

  private[this] def fromQualId
      (context: EECParser.QualIdContext) given IdGen: Tree = {
    val tokens = {
      import implied NameOps._
      import scala.language.implicitConversions
      context.id.map(_.getText.readAs).toList.reverse
    }
    namesToTree(tokens)
  }

  private[this] def fromStableId
      (context: EECParser.StableIdContext) given IdGen: Tree = {
    val ids = {
      import scala.language.implicitConversions
      context.id.map(fromId).ensuring(result.size <= 2)
    }
    if ids.size == 1 then {
      ids(0)
    } else {
      import implied TreeOps._
      namesToTree(ids.toList.reverse.map(Convert[Tree, Name]))
    }
  }

  private[this] def fromType(context: EECParser.TypeContext) given IdGen: Tree =
    if defined(context.func) then
      fromFunc(context.func)
    else
      fromInfixType(context.infixType)

  private[this] def fromFunc
      (context: EECParser.FuncContext) given IdGen: Tree = {
    import scala.language.implicitConversions
    val infixes = context
      .infixType
      .map(fromInfixType)
      .ensuring(result.size >= 2)
      .toList
    val head :: rest = infixes.reverse
    rest.foldLeft(head)((acc, t) => Function(List(t), acc)(freshId(), uTpe))
  }

  private[this] def fromInfixType
      (context: EECParser.InfixTypeContext) given IdGen: Tree =
    if defined(context.simpleType) then
      fromSimpleType(context.simpleType)
    else
      fromPrefixType(context.prefixType)

  private[this] def fromProductType
        (context: EECParser.ProductTypeContext) given IdGen: Tree = {
    import scala.language.implicitConversions
    val types = context.`type`.map(fromType)
    if types.size == 1 then
      types(0)
    else
      Parens(types.toList)(freshId(), uTpe)
  }

  private[this] def fromPrefixType
        (context: EECParser.PrefixTypeContext) given IdGen: Tree = {
    if defined(context.Bang) then
      fromBangType(context)
    else
      fromFunctorType(context)
  }

  private[this] def fromFunctorType
        (context: EECParser.PrefixTypeContext) given IdGen: Tree = {
    val simpleTypes = {
      import scala.language.implicitConversions
      context.simpleType.map(fromSimpleType)
    }
    val tag   = fromQualId(context.qualId)
    val args  = simpleTypes.toList
    Apply(tag, args)(freshId(), uTpe)
  }

  private[this] def fromBangType
        (context: EECParser.PrefixTypeContext) given IdGen: Tree = {
    val simpleTypes = {
      import scala.language.implicitConversions
      context.simpleType.map(fromSimpleType)
    }
    val tag   = Ident(Name.ComputationTag)(freshId(), uTpe)
    val args  = simpleTypes.toList
    Apply(tag, args)(freshId(), uTpe)
  }

  private[this] def fromSimpleType
      (context: EECParser.SimpleTypeContext) given IdGen: Tree =
    if defined(context.qualId) then
      fromQualId(context.qualId)
    else
      fromProductType(context.productType)

  private[this] def fromExpr(context: EECParser.ExprContext) given IdGen: Tree =
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

  private[this] def fromExprSeqAsApply
        (exprs: java.util.List[EECParser.ExprContext]) given IdGen: Tree = {
    import scala.language.implicitConversions
    val Seq(expr, arg)  = exprs.map(fromExpr).ensuring(result.size == 2)
    val args            = toList(arg)
    Apply(expr, args)(freshId(), uTpe)
  }

  private[this] def fromLambda
      (context: EECParser.LambdaContext) given IdGen: Tree = {
    val bindings  = toList(fromBindings(context.bindings))
    val body      = fromExpr(context.expr)
    Function(bindings, body)(freshId(), uTpe)
  }

  private[this] def fromLetExpr
      (context: EECParser.LetExprContext) given IdGen: Tree = {
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

  private[this] def fromCaseExpr
      (context: EECParser.CaseExprContext) given IdGen: Tree = {
    val selector = fromExpr(context.expr)
    val cases = toList(fromCases(context.cases))
    CaseExpr(selector, cases)(freshId(), uTpe)
  }

  private[this] def fromIfElse
      (context: EECParser.Expr1Context)  given IdGen: Tree = {
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

  private[this] def fromExpr1
      (context: EECParser.Expr1Context) given IdGen: Tree =
    if defined(context.infixExpr) then
      fromInfixExpr(context.infixExpr)
    else
      fromIfElse(context)

  private[this] def fromInfixApplication
        (context: EECParser.InfixExprContext) given IdGen: Tree = {
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

  private[this] def fromInfixExpr
      (context: EECParser.InfixExprContext) given IdGen: Tree =
    if defined(context.prefixExpr) then
      fromPrefixExpr(context.prefixExpr)
    else
      fromInfixApplication(context)

  private[this] def wrapComputation(tree: Tree) given IdGen: Tree = {
    import Name._
    val tag = Ident(ComputationTag)(freshId(), uTpe)
    Apply(tag, List(tree))(freshId(), uTpe)
  }

  private[this] def fromPrefixExpr
      (context: EECParser.PrefixExprContext) given IdGen: Tree = {
    val simpleExpr = fromSimpleExpr(context.simpleExpr)
    if defined(context.Bang) then
      wrapComputation(simpleExpr)
    else
      simpleExpr
  }

  private[this] def fromSimpleExpr
      (context: EECParser.SimpleExprContext) given IdGen: Tree =
    if defined(context.literal) then
      fromLiteral(context.literal)
    else if defined(context.stableId) then
      fromStableId(context.stableId)
    else
      fromExprsInParens(context.exprsInParens)

  private[this] def fromCases
      (context: EECParser.CasesContext) given IdGen: Tree = {
    val caseClauses = {
      import scala.language.implicitConversions
      context.caseClause.map(fromCaseClause).toList
    }
    fromList(caseClauses)
  }

  private[this] def fromCaseClause
      (context: EECParser.CaseClauseContext) given IdGen: Tree = {
    val pat = fromPattern(context.pattern)
    val guard =
      if defined(context.guard) then
        fromGuard(context.guard)
      else
        EmptyTree
    val body = fromExpr(context.expr)
    CaseClause(pat, guard, body)(freshId(), uTpe)
  }

  private[this] def fromExprsInParens
      (context: EECParser.ExprsInParensContext) given IdGen: Tree = {
    val expr = {
      import scala.language.implicitConversions
      context.expr.map(fromExpr)
    }
    if expr.size == 1 then
      expr(0)
    else
      Parens(expr.toList)(freshId(), uTpe)
  }

  private[this] def fromPattern
      (context: EECParser.PatternContext) given IdGen: Tree = {
    val patterns = {
      import scala.language.implicitConversions
      context.pattern1.map(fromPattern1)
    }
    if patterns.size == 1 then
      patterns(0)
    else
      Alternative(patterns.toList)(freshId(), uTpe)
  }

  private[this] def fromPattern1
      (context: EECParser.Pattern1Context) given IdGen: Tree =
    fromPattern2(context.pattern2)

  private[this] def fromBind
      (context: EECParser.Pattern2Context) given IdGen: Tree = {
    val name = {
      import implied NameOps._
      context.Varid.getText.readAs
    }
    if defined(context.pattern3) then
      Bind(name, fromPattern3(context.pattern3))(freshId(), uTpe)
    else
      Ident(name)(freshId(), uTpe)
  }

  private[this] def fromPattern2
      (context: EECParser.Pattern2Context) given IdGen: Tree =
    if defined(context.Varid) then
      fromBind(context)
    else
      fromPattern3(context.pattern3)

  private[this] def fromPattern3
      (context: EECParser.Pattern3Context) given IdGen: Tree =
    fromSimplePattern(context.simplePattern)

  private[this] def fromVaridPattern
      (context: EECParser.SimplePatternContext) given IdGen: Tree = {
    val name = {
      import implied NameOps._
      context.Varid.getText.readAs
    }
    Ident(name)(freshId(), uTpe)
  }

  private[this] def fromFunctorPattern
      (context: EECParser.SimplePatternContext) given IdGen: Tree = {
    val functor = {
      import implied NameOps._
      context.Patid.getText.readAs
    }
    val args = {
      import scala.language.implicitConversions
      context.pattern.map(fromPattern).toList
    }
    Unapply(functor, args)(freshId(), uTpe)
  }

  private[this] def fromUnitPattern given IdGen: Tree =
    Parens(Nil)(freshId(), uTpe)

  private[this] def fromSimplePattern
      (context: EECParser.SimplePatternContext) given IdGen: Tree =
    if defined(context.Wildcard) then
      any.wildcardIdent
    else if context.getText == "()" then
      fromUnitPattern
    else if defined(context.Varid) then
      fromVaridPattern(context)
    else if defined(context.literal) then
      fromLiteral(context.literal)
    else if defined(context.Patid) then
      fromFunctorPattern(context)
    else
      fromPatterns(context.patterns)

  private[this] def fromPatterns
      (context: EECParser.PatternsContext) given IdGen: Tree = {
    val patterns = {
      import scala.language.implicitConversions
      context.pattern.map(fromPattern)
    }
    if patterns.size == 1 then
      patterns(0)
    else
      Parens(patterns.toList)(freshId(), uTpe)
  }

  private[this] def fromGuard
      (context: EECParser.GuardContext) given IdGen: Tree =
    fromInfixExpr(context.infixExpr)

  private[this] def fromBindings
      (context: EECParser.BindingsContext) given IdGen: Tree =
    fromBindingsTagged(context.bindingsTagged)

  private[this] def fromBindingsTagged
      (context: EECParser.BindingsTaggedContext) given IdGen: Tree = {
    val bindings = {
      import scala.language.implicitConversions
      context.binding.map(fromBinding).toList
    }
    fromList(bindings)
  }

  private[this] def fromBinding
      (context: EECParser.BindingContext) given IdGen: Tree = {
    import TreeOps._
    val List(name) = fromId(context.id).toNames
    val typ = fromType(context.`type`)
    Tagged(name, typ)(freshId(), uTpe)
  }

  private[this] def fromDcl(context: EECParser.DclContext) given IdGen: Tree =
    fromPrimitiveDcl(context.primitiveDcl)

  private[this] def fromPrimitiveDcl
      (context: EECParser.PrimitiveDclContext) given IdGen: Tree = {
    import TreeOps._
    val modifiers = Set(Primitive)
    fromDefDecl(context.defDecl).addModifiers(modifiers)
  }

  private[this] def fromDefDecl
      (context: EECParser.DefDeclContext) given IdGen: Tree = {
    val sig = fromDefSig(context.defSig)
    val typ = fromType(context.`type`)
    DefDef(Set(), sig, typ, EmptyTree)(freshId(), uTpe)
  }

  private[this] def fromDef
      (context: EECParser.DefContext) given IdGen: Tree =
    fromDefDef(context.defDef)

  private[this] def fromDefDef
      (context: EECParser.DefDefContext) given IdGen: Tree = {
    val defSig  = fromDefSig(context.defSig)
    val typ     = fromType(context.`type`)
    val expr    = fromExpr(context.expr)
    DefDef(Set(), defSig, typ, expr)(freshId(), uTpe)
  }

  private[this] def fromDefSig
      (context: EECParser.DefSigContext) given IdGen: Tree =
    if defined(context.infixDefSig) then
      fromInfixDefSig(context.infixDefSig)
    else
      fromPrefixDefSig(context)

  private[this] def fromPrefixDefSig
      (context: EECParser.DefSigContext) given IdGen: Tree = {
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

  private[this] def fromInfixDefSig
      (context: EECParser.InfixDefSigContext) given IdGen: Tree =
    if defined(context.prefixOpSig) then
      fromPrefixOpSig(context.prefixOpSig)
    else if defined(context.OpId) then
      fromInfixOpSig(context)
    else
      fromInfixAlphaSig(context)

  private[this] def infixArgs
      (context: EECParser.InfixDefSigContext) given IdGen: List[Tree] = {
    import scala.language.implicitConversions
    import implied NameOps._
    context.Varid
      .ensuring(result.size == 2)
      .map(_.getText.readAs)
      .map(Ident(_)(freshId(), uTpe))
      .toList
  }

  private[this] def fromInfixOpSig
      (context: EECParser.InfixDefSigContext) given IdGen: Tree = {
    var args = infixArgs(context)
    var name = {
      import implied NameOps._
      context.OpId.getText.readAs
    }
    DefSig(name, args)(freshId(), uTpe)
  }

  private[this] def fromInfixAlphaSig
      (context: EECParser.InfixDefSigContext) given IdGen: Tree = {
    var args = infixArgs(context)
    val name = {
      import implied TreeOps._
      Convert[Tree, Name](fromAlphaId(context.alphaId))
    }
    DefSig(name, args)(freshId(), uTpe)
  }

  private[this] def fromPrefixOpSig
      (context: EECParser.PrefixOpSigContext) given IdGen: Tree = {
    import scala.language.implicitConversions
    import implied NameOps._
    var args = context.Varid
      .map { _.getText.readAs }
      .map { Ident(_)(freshId(), uTpe) }
      .toList
    val name = context.OpId.getText.readAs
    DefSig(name, args)(freshId(), uTpe)
  }

  private[this] def fromPackageInfo
      (context: EECParser.PackageInfoContext) given IdGen: Tree =
    fromQualId(context.qualId)

  private[this] def fromStatSeq
      (context: EECParser.StatSeqContext) given IdGen: Tree = {
    import scala.language.implicitConversions
    import implied TreeOps._
    val stats = context.stat.map(fromStat).toList
    fromList(stats)
  }

  private[this] def fromStat(context: EECParser.StatContext) given IdGen: Tree =
    if defined(context.`def`) then
      fromDef(context.`def`)
    else
      fromDcl(context.dcl)

  private[this] val eecErrorListener: BaseErrorListener = new {
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

  private[this] def genParser(input: String): EECParser = {
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