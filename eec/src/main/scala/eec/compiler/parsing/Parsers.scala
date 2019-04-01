package eec
package compiler
package parsing

import scala.language.implicitConversions

import scala.collection.JavaConversions._
import scala.collection.mutable

import ast._
import core._
import Trees.{Tree, TreeOps}
import Tree._
import TreeOps._
import untyped._
import Names._
import Name._
import NameOps._
import Contexts._
import IdGen._
import Constants.{Constant}
import Constant._
import Modifiers.{Modifier}
import Modifier._
import EECParser._
import types.Types.TypeOps._
import util.{Convert, |>}
import Convert._
import error.CompilerErrors._

import org.antlr.v4.runtime._

import implied TreeOps._
import implied NameOps._

object Parsers {

  private[parsing] val eecParser =
    genParser `andThen` { _.translationUnit }

  private[parsing] val statParser =
    genParser `andThen` { _.statAsTop }

  private[parsing] val exprParser =
    genParser `andThen` { _.exprAsTop }

  private val unit: Tree =
    Parens(Nil)(uTpe)

  private[parsing] def fromStatAsTop(context: StatAsTopContext)
                                    given IdGen: Checked[Tree] =
    fromStat(context.stat)

  private[parsing] def fromExprAsTop(context: ExprAsTopContext)
                                    given IdGen: Checked[Tree] =
    fromExpr(context.expr)

  private[parsing] def fromTranslationUnit(context: TranslationUnitContext)
                                          given IdGen: Checked[Tree] = {
    import CompilerErrorOps._
    for {
      pkgId <- fromPackageInfo(context.packageInfo)
      stats <- checked {
                if defined(context.statSeq) then
                  fromStatSeq(context.statSeq)
                else
                  EmptyTree
               }
    } yield PackageDef(pkgId, stats.convert)(uTpe)
  }

  private class ParserSyntaxException(msg: String) extends Exception(msg)

  private inline def defined[A <: AnyRef](a: A): Boolean = a `ne` null

  private def freshId() given IdGen: Id = idGen.fresh()

  def (o: String => O) toTreeParser[O]
      (f: IdReader[O => Checked[Tree]])
      (input: String) given IdGen = {
    import CompilerErrorOps._
    val lifted = o(input).recover {
      case e: ParserSyntaxException =>
        CompilerError.SyntaxError(e.getMessage)
    }
    lifted.flatMap(f)
  }

  private def fromIntegerLiteral(context: LiteralContext): Checked[Tree] = {
    val txt = context.IntegerLiteral.getText
    if txt.endsWith("l") || txt.endsWith("L") then
      CompilerError.SyntaxError(s"unexpected Long literal `$txt`")
    else
      Literal(BigIntConstant(BigInt(txt)))(uTpe)
  }

  private def fromFloatingPointLiteral(context: LiteralContext): Checked[Tree] = {
    val txt = context.FloatingPointLiteral.getText
    if txt.endsWith("f") || txt.endsWith("F") then
      CompilerError.SyntaxError(s"unexpected Float literal `$txt`")
    else if txt.endsWith("d") || txt.endsWith("D") then
      CompilerError.SyntaxError(s"unexpected Double literal `$txt`")
    else
      Literal(BigDecConstant(BigDecimal(txt)))(uTpe)
  }

  private def fromBooleanLiteral(context: LiteralContext): Tree = {
    val bool = context.BooleanLiteral.getText match {
      case "True" => true
      case _      => false
    }
    Literal(BooleanConstant(bool))(uTpe)
  }

  private def charText(context: LiteralContext): String = {
    context.CharacterLiteral
      .getText
      .stripPrefix("'").stripSuffix("'")
  }

  private def formatString(text: String): String = {
    if text startsWith "\"\"\"" then
      text stripPrefix "\"\"\"" stripSuffix "\"\"\""
    else
      text stripPrefix "\"" stripSuffix "\""
  }

  private def fromCharacterLiteral(context: LiteralContext): Tree = {
    val charStr = charText(context)
    val char    = if charStr.length == 0 then 0 else charStr.charAt(0)
    Literal(CharConstant(char))(uTpe)
  }

  private def fromStringLiteral(context: LiteralContext): Tree = {
    val text    = context.StringLiteral.getText
    val string  = formatString(text)
    Literal(StringConstant(string))(uTpe)
  }

  private def fromLiteral(context: LiteralContext): Checked[Tree] = {
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
  }

  private def fromId(context: IdContext) given IdGen: Tree = {
    if defined(context.alphaId) then
      fromAlphaId(context.alphaId)
    else
      fromOpId(context)
  }

  private def fromOpId(context: IdContext) given IdGen: Tree = {
    val name = context.OpId.getText.readAs
    Ident(name)(freshId(), uTpe)
  }

  private def fromAlphaId(context: AlphaIdContext) given IdGen: Tree = {
    val name = context.getText.readAs
    Ident(name)(freshId(), uTpe)
  }

  private def namesToTree(lst: List[Name]) given IdGen: Tree = lst match {
    case n :: Nil   => Ident(n)(freshId(), uTpe)
    case n :: tail  => Select(namesToTree(tail), n)(freshId(), uTpe)
    case Nil        => EmptyTree
  }

  private def fromQualId(context: QualIdContext) given IdGen: Tree = {
    val tokens = {
      context
        .id
        .map(_.getText.readAs)
        .toList
    }
    namesToTree(tokens.reverse)
  }

  private def fromStableId(context: StableIdContext) given IdGen: Tree = {
    val ids = {
      context
        .id
        .map(fromId)
    }
    if ids.size == 1 then
      ids(0)
    else
      namesToTree(ids.toList.reverse.map(_.convert: Name))
  }

  private def fromType(context: TypeContext)
                      given IdGen: Checked[Tree] = {
    if defined(context.func) then
      fromFunc(context.func)
    else if defined(context.linearFunc) then
      fromLinearFunc(context.linearFunc)
    else
      fromInfixType(context.infixType)
  }

  private def fromInfixType(context: InfixTypeContext)
                           given IdGen: Checked[Tree] = {
    if defined(context.functorType) then
      fromFunctorType(context.functorType)
    else
      fromInfixAppliedType(context.infixAppliedType)
  }

  private def fromInfixAppliedType(context: InfixAppliedTypeContext)
                                  given IdGen: Checked[Tree] = {
    import CompilerErrorOps._
    val name = context.rassocOpId.getText.readAs
    val functor = Ident(name)(freshId(), uTpe)
    for {
      functor1 <- fromFunctorType(context.functorType)
      iat      <- fromInfixType(context.infixType)
    } yield iat match {
      case InfixApply(Ident(other),_,_) if other != name && !context.infixType.getText.startsWith("(") =>
        CompilerError.SyntaxError(s"Non associative rhs `${other.show}` to infix type `${name.show}`")
      case _ =>
        InfixApply(functor, functor1, iat)(uTpe)
    }
  }

  private def fromLinearFunc(context: LinearFuncContext)
                            given IdGen: Checked[Tree] = {
    import CompilerErrorOps._
    for {
      infixes <- checked {
        context
          .infixType
          .mapE(fromInfixType)
      }
    } yield LinearFunction(infixes(0), infixes(1))(freshId(), uTpe)
  }

  private def fromFunc(context: FuncContext) given IdGen: Checked[Tree] = {
    import CompilerErrorOps._
    for {
      infixes <- checked {
        context
          .infixType
          .mapE(fromInfixType)
      }
    } yield {
      val head :: rest = infixes.toList.reverse
      rest.foldLeft(head)((acc, t) => Function(List(t), acc)(freshId(), uTpe))
    }
  }

  private def fromFunctorType(context: FunctorTypeContext) given IdGen: Checked[Tree] = {
    if defined(context.simpleType) then
      fromSimpleType(context.simpleType)
    else
      fromPrefixType(context.prefixType)
  }

  private def fromProductType(context: ProductTypeContext)
                             given IdGen: Checked[Tree] = {
    import CompilerErrorOps._
    for (types <- context.`type`.mapE(fromType))
    yield parensFromBuffer(types)
  }

  private def fromPrefixType(context: PrefixTypeContext) given IdGen: Checked[Tree] = {
    if defined(context.Bang) then
      fromBangType(context)
    else
      fromFunctorType(context)
  }

  private def fromFunctorType(context: PrefixTypeContext) given IdGen: Checked[Tree] = {
    import CompilerErrorOps._
    for (simpleTypes <- context.simpleType.mapE(fromSimpleType)) yield {
      val tag         = fromQualId(context.qualId)
      val args        = simpleTypes.toList
      Apply(tag, args)(uTpe)
    }
  }

  private def fromBangType(context: PrefixTypeContext) given IdGen: Checked[Tree] = {
    import CompilerErrorOps._
    val tag         = Ident(Name.BangTag)(freshId(), uTpe)
    for (simpleTypes <- context.simpleType.mapE(fromSimpleType)) yield {
      val args        = simpleTypes.toList
      Apply(tag, args)(uTpe)
    }
  }

  private def fromSimpleType(context: SimpleTypeContext)
                            given IdGen: Checked[Tree] = {
    if defined(context.qualId) then
      fromQualId(context.qualId)
    else if defined(context.CompId) then
      fromCompId(context)
    else
      fromProductType(context.productType)
  }

  private def fromCompId(context: SimpleTypeContext) given IdGen: Tree =
    Ident(context.CompId.getText.readAs.promoteComp)(freshId(), uTpe)

  private def fromExpr(context: ExprContext) given IdGen: Checked[Tree] = {
    if defined(context.lambda) then
      fromLambda(context.lambda)
    else if defined(context.linearLambda) then
      fromLinearLambda(context.linearLambda)
    else if defined(context.letExpr) then
      fromLetExpr(context.letExpr)
    else if defined(context.letTensorExpr) then
      fromLetTensorExpr(context.letTensorExpr)
    else if defined(context.caseExpr) then
      fromCaseExpr(context.caseExpr)
    else if defined(context.linearCaseExpr) then
      fromLinearCaseExpr(context.linearCaseExpr)
    else if defined(context.expr1) then
      fromExpr1(context.expr1)
    else if defined(context.eval) then
      fromEval(context.expr(0), context.eval)
    else
      fromExprSeqAsApply(context.expr)
  }

  private def fromEval(exprContext: ExprContext, evalContext: EvalContext)
                      given IdGen: Checked[Tree] = {
    import CompilerErrorOps._
    for {
      expr <- fromExpr(exprContext)
      eval <- fromExpr(evalContext.expr)
    } yield Eval(expr, eval)(uTpe)
  }

  private def fromExprSeqAsApply(exprs: java.util.List[ExprContext])
                                given IdGen: Checked[Tree] = {
    import CompilerErrorOps._
    for {
      exprs <- exprs.mapE(fromExpr)
      Seq(expr, arg) = exprs
    } yield Apply(expr, arg.convert)(uTpe)
  }

  private def fromLambda(context: LambdaContext) given IdGen: Checked[Tree] = {
    import CompilerErrorOps._
    for {
      body     <- fromExpr(context.expr)
      bindings <- fromBindings(context.bindings)
    } yield Function(bindings.convert, body)(freshId(), uTpe)
  }

  private def fromLinearLambda(context: LinearLambdaContext)
                              given IdGen: Checked[Tree] = {
    import CompilerErrorOps._
    for {
      body <- fromExpr(context.expr)
      binding <- fromBinding(context.binding)
    } yield LinearFunction(binding, body)(freshId(), uTpe)
  }

  private def fromLetExpr(context: LetExprContext) given IdGen: Checked[Tree] = {
    import CompilerErrorOps._
    var name = {
      if defined(context.Varid) then
        context.Varid.getText.readAs
      else
        Name.Wildcard
    }
    for (exprs <- context.expr.mapE(fromExpr)) yield {
      val value         = exprs.get(0)
      val continuation  = exprs.get(1)
      Let(name, value, continuation)(freshId(), uTpe)
    }
  }

  private def fromLetTensorExpr(context: LetTensorExprContext)
                               given IdGen: Checked[Tree] = {
    var varids = context.Varid.map(_.getText.readAs)
    var x = {
      if defined(context.Wildcard) then
        Name.Wildcard
      else
        varids(0)
    }
    val z = varids.last
    import CompilerErrorOps._
    for (exprs <- context.expr.mapE(fromExpr)) yield {
      val value         = exprs.get(0)
      val continuation  = exprs.get(1)
      LetTensor(x, z, value, continuation)(freshId(), uTpe)
    }
  }

  private def fromCaseExpr(context: CaseExprContext)
                          given IdGen: Checked[Tree] = {
    import CompilerErrorOps._
    for {
      selector  <- fromExpr(context.expr)
      cases     <- fromCases(context.cases)
    } yield CaseExpr(selector, cases.convert)(uTpe)
  }

  private def fromLinearCaseExpr(context: LinearCaseExprContext)
                                given IdGen: Checked[Tree] = {
    import CompilerErrorOps._
    for {
      selector  <- fromExpr(context.expr)
      cases     <- fromLinearCases(context.linearCases)
    } yield LinearCaseExpr(selector, cases.convert)(uTpe)
  }

  private def fromIfElse(context: Expr1Context) given IdGen: Checked[Tree] = {
    import CompilerErrorOps._
    for (exprs <- context.expr.mapE(fromExpr)) yield {
      val patTrue   = Literal(BooleanConstant(true))(uTpe)
      val patFalse  = Literal(BooleanConstant(false))(uTpe)
      val caseTrue  = CaseClause(patTrue, EmptyTree, exprs(1))(freshId(), uTpe)
      val caseFalse = CaseClause(patFalse, EmptyTree, exprs(2))(freshId(), uTpe)
      val selector  = exprs(0)
      CaseExpr(selector, List(caseTrue, caseFalse))(uTpe)
    }
  }

  private def fromExpr1(context: Expr1Context) given IdGen: Checked[Tree] = {
    if defined(context.infixExpr) then
      fromInfixExpr(context.infixExpr)
    else
      fromIfElse(context)
  }

  private def fromInfixApplication(context: InfixExprContext)
                                  given IdGen: Checked[Tree] = {
    import CompilerErrorOps._
    val id = {
      if defined(context.OpId) then
        Ident(context.OpId.getText.readAs)(freshId(), uTpe)
      else
        fromAlphaId(context.alphaId)
    }
    for (infixes <- context.infixExpr.mapE(fromInfixExpr)) yield {
      val firstApply = Apply(id, List(infixes(0)))(uTpe)
      Apply(firstApply, List(infixes(1)))(uTpe)
    }
  }

  private def fromInfixExpr(context: InfixExprContext)
                           given IdGen: Checked[Tree] = {
    if defined(context.prefixExpr) then
      fromPrefixExpr(context.prefixExpr)
    else if defined(context.tensorExpr) then
      fromTensorExpr(context.tensorExpr)
    else
      fromInfixApplication(context)
  }

  private def wrapBang(tree: Tree): Checked[Tree] =
    Tree.Bang(tree)(uTpe)

  private def wrapWhyNot(tree: Tree): Checked[Tree] =
    Tree.WhyNot(tree)(uTpe)

  private def fromTensorExpr(context: TensorExprContext)
                            given IdGen: Checked[Tree] = {
    import CompilerErrorOps._
    for {
      x <- fromSimpleExpr(context.simpleExpr)
      z <- fromInfixExpr(context.infixExpr)
    } yield Tree.Tensor(x, z)(uTpe)
  }

  private def fromPrefixExpr(context: PrefixExprContext)
                            given IdGen: Checked[Tree] = {
    import CompilerErrorOps._
    val simpleExpr = fromSimpleExpr(context.simpleExpr)
    if defined(context.Bang) then
      simpleExpr.map(wrapBang)
    else if defined(context.WhyNot) then
      simpleExpr.map(wrapWhyNot)
    else
      simpleExpr
  }

  private def fromSimpleExpr(context: SimpleExprContext)
                            given IdGen: Checked[Tree] = {
    if defined(context.literal) then
      fromLiteral(context.literal)
    else if defined(context.stableId) then
      fromStableId(context.stableId)
    else
      fromExprsInParens(context.exprsInParens)
  }

  private def fromCases(context: CasesContext)
                       given IdGen: Checked[Tree] = {
    import CompilerErrorOps._
    for (caseClauses <- context.caseClause.mapE(fromCaseClause))
    yield caseClauses.toList.convert
  }

  private def fromLinearCases(context: LinearCasesContext)
                             given IdGen: Checked[Tree] = {
    import CompilerErrorOps._
    for (caseClauses <- context.linearCaseClause.mapE(fromLinearCaseClause))
    yield caseClauses.toList.convert
  }

  private def fromCaseClause(context: CaseClauseContext)
                            given IdGen: Checked[Tree] = {
    import CompilerErrorOps._
    for {
      pat   <- fromPattern(context.pattern)
      guard <- checked {
        if defined(context.guard) then
          fromGuard(context.guard)
        else
          EmptyTree
      }
      body <- fromExpr(context.expr)
    } yield CaseClause(pat, guard, body)(freshId(), uTpe)
  }

  private def fromLinearCaseClause(context: LinearCaseClauseContext)
                                  given IdGen: Checked[Tree] = {
    import CompilerErrorOps._
    for {
      pat  <- fromLinearPattern(context.linearPattern)
      body <- fromExpr(context.expr)
    } yield LinearCaseClause(pat, body)(freshId(), uTpe)
  }

  private def fromExprsInParens(context: ExprsInParensContext)
                               given IdGen: Checked[Tree] = {
    import CompilerErrorOps._
    for (exprs <- context.expr.mapE(fromExpr))
    yield parensFromBuffer(exprs)
  }

  private def fromLinearPattern(context: LinearPatternContext)
                               given IdGen: Tree = {
    if defined(context.Wildcard) then
      any.wildcardIdent
    else if defined(context.Varid) then
      fromVaridLinearPattern(context)
    else if defined(context.linearPattern) then
      fromLinearUnapply(context)
    else if defined(context.linearPatterns) then
      fromLinearPatterns(context.linearPatterns)
    else
      unit
  }

  private def fromLinearUnapply(context: LinearPatternContext)
                               given IdGen: Tree = {
    import CompilerErrorOps._
    val functor = context.Patid.getText.readAs
    val binding = fromLinearPattern(context.linearPattern)
    Unapply(functor, List(binding))(uTpe)
  }

  private def fromLinearPatterns(context: LinearPatternsContext)
                                given IdGen: Tree = {
    val patterns = context.linearPattern.map(fromLinearPattern)
    parensFromBuffer(patterns)
  }

  private def fromVaridLinearPattern(context: LinearPatternContext)
                                    given IdGen: Tree = {
    val name = context.Varid.getText.readAs
    Ident(name)(freshId(), uTpe)
  }

  private def fromPattern(context: PatternContext)
                         given IdGen: Checked[Tree] = {
    import CompilerErrorOps._
    for (patterns <- context.pattern1.mapE(fromPattern1)) yield {
      if patterns.size == 1 then
        patterns(0)
      else
        Alternative(patterns.toList)(uTpe)
    }
  }

  private def fromPattern1(context: Pattern1Context) given IdGen: Checked[Tree] =
    fromPattern2(context.pattern2)

  private def fromBind(context: Pattern2Context) given IdGen: Checked[Tree] = {
    import CompilerErrorOps._
    val name = context.Varid.getText.readAs
    if defined(context.pattern3) then {
      for (patt3 <- fromPattern3(context.pattern3))
      yield Bind(name, patt3)(uTpe)
    } else {
      Ident(name)(freshId(), uTpe)
    }
  }

  private def fromPattern2(context: Pattern2Context) given IdGen: Checked[Tree] = {
    if defined(context.Varid) then
      fromBind(context)
    else
      fromPattern3(context.pattern3)
  }

  private def fromPattern3(context: Pattern3Context) given IdGen: Checked[Tree] =
    fromSimplePattern(context.simplePattern)

  private def fromVaridPattern(context: SimplePatternContext) given IdGen: Tree = {
    val name = context.Varid.getText.readAs
    Ident(name)(freshId(), uTpe)
  }

  private def fromFunctorPattern(context: SimplePatternContext)
                                given IdGen: Checked[Tree] = {
    import CompilerErrorOps._
    val functor = context.Patid.getText.readAs
    for (args <- context.pattern.mapE(fromPattern))
    yield Unapply(functor, args.toList)(uTpe)
  }

  private def fromSimplePattern(context: SimplePatternContext)
                               given IdGen: Checked[Tree] = {
    if defined(context.Wildcard) then
      any.wildcardIdent
    else if context.getText == "()" then
      unit
    else if defined(context.Varid) then
      fromVaridPattern(context)
    else if defined(context.literal) then
      fromLiteral(context.literal)
    else if defined(context.Patid) then
      fromFunctorPattern(context)
    else
      fromPatterns(context.patterns)
  }

  private def fromPatterns(context: PatternsContext) given IdGen: Checked[Tree] = {
    import CompilerErrorOps._
    for (patterns <- context.pattern.mapE(fromPattern))
    yield parensFromBuffer(patterns)
  }

  private def parensFromBuffer(trees: mutable.Buffer[Tree]): Tree =
    if trees.size == 1 then trees(0)
    else Parens(trees.toList)(uTpe)

  private def fromGuard(context: GuardContext) given IdGen: Checked[Tree] =
    fromInfixExpr(context.infixExpr)

  private def fromBindings(context: BindingsContext) given IdGen: Checked[Tree] =
    fromBindingsTagged(context.bindingsTagged)

  private def fromBindingsTagged(context: BindingsTaggedContext)
                                given IdGen: Checked[Tree] = {
    import CompilerErrorOps._
    for (bindings <- context.binding.mapE(fromBinding))
    yield bindings.toList.convert
  }

  private def fromBinding(context: BindingContext) given IdGen: Checked[Tree] = {
    import CompilerErrorOps._
    val name: Name  = {
      if defined(context.id) then
        fromId(context.id).convert
      else
        Name.Wildcard
    }
    for (typ <- fromType(context.`type`))
    yield Tagged(name, typ)(uTpe)
  }

  private def fromDcl(context: DclContext) given IdGen: Checked[Tree] =
    fromPrimitiveDcl(context.primitiveDcl)

  private def fromPrimitiveDcl(context: PrimitiveDclContext) given IdGen: Checked[Tree] = {
    import CompilerErrorOps._
    fromPrimDcl(context.primDecl).map(_.addModifiers(Set(Primitive)))
  }

  private def fromPrimDcl(context: PrimDeclContext) given IdGen: Checked[Tree] = {
    import CompilerErrorOps._
    val sig = {
      if defined(context.defSig) then
        fromDefSig(context.defSig)
      else
        fromLinearSig(context.linearSig)
    }
    for (typ <- fromType(context.`type`))
    yield DefDef(Set(), sig, typ, EmptyTree)(uTpe)
  }

  private def fromLinearSig(context: LinearSigContext) given IdGen: Tree = {
    var name     = (fromAlphaId(context.alphaId).convert: Name)
    val paramids = context.paramName.map(fromParamName)
    if paramids.size == 1 then
      LinearSig(name, Nil, paramids(0))(freshId(), uTpe)
    else
      LinearSig(name, paramids.init.toList, paramids.last)(freshId(), uTpe)
  }

  private def fromParamName(context: ParamNameContext): Name =
    if defined(context.Wildcard) then Name.Wildcard
    else context.Varid.getText.readAs

  private def fromDef(context: DefContext) given IdGen: Checked[Tree] =
    fromDefDef(context.defDef)

  private def fromDefDef(context: DefDefContext) given IdGen: Checked[Tree] = {
    import CompilerErrorOps._
    for {
      expr    <- fromExpr(context.expr)
      typ     <-  fromType(context.`type`)
      sig  = {
        if defined(context.defSig) then
          fromDefSig(context.defSig)
        else
          fromLinearSig(context.linearSig)
      }
    } yield DefDef(Set(), sig, typ, expr)(uTpe)
  }

  private def fromDefSig(context: DefSigContext) given IdGen: Tree = {
    if defined(context.infixDefSig) then
      fromInfixDefSig(context.infixDefSig)
    else
      fromPrefixDefSig(context)
  }

  private def fromPrefixDefSig(context: DefSigContext) given IdGen: Tree = {
    var name      = (fromAlphaId(context.alphaId).convert: Name)
    var paramids  = context.paramName.map(fromParamName).toList
    DefSig(name, paramids)(freshId(), uTpe)
  }

  private def fromInfixDefSig(context: InfixDefSigContext) given IdGen: Tree = {
    if defined(context.prefixOpSig) then
      fromPrefixOpSig(context.prefixOpSig)
    else if defined(context.OpId) then
      fromInfixOpSig(context)
    else
      fromInfixAlphaSig(context)
  }

  private def fromInfixOpSig(context: InfixDefSigContext) given IdGen: Tree = {
    var args = context.paramName.map(fromParamName).toList
    var name = context.OpId.getText.readAs
    DefSig(name, args)(freshId(), uTpe)
  }

  private def fromInfixAlphaSig(context: InfixDefSigContext) given IdGen: Tree = {
    var args  = context.paramName.map(fromParamName).toList
    val name  = (fromAlphaId(context.alphaId).convert: Name)
    DefSig(name, args)(freshId(), uTpe)
  }

  private def fromPrefixOpSig(context: PrefixOpSigContext) given IdGen: Tree = {
    var args = context.paramName.map(fromParamName).toList
    val name = context.OpId.getText.readAs
    DefSig(name, args)(freshId(), uTpe)
  }

  private def fromPackageInfo(context: PackageInfoContext) given IdGen: Tree =
    fromQualId(context.qualId)

  private def fromStatSeq(context: StatSeqContext) given IdGen: Checked[Tree] = {
    import CompilerErrorOps._
    for (stats <- context.stat.mapE(fromStat))
    yield stats.toList.convert
  }

  private def fromStat(context: StatContext) given IdGen: Checked[Tree] = {
    if defined(context.`def`) then
      fromDef(context.`def`)
    else
      fromDcl(context.dcl)
  }

  private val eecErrorListener: BaseErrorListener = new {
    override def syntaxError(
        recognizer: Recognizer[_,_],
        offendingSymbol: AnyRef,
        line: Int,
        charPositionInLine: Int,
        msg: String,
        e: RecognitionException): Unit = {
      throw new ParserSyntaxException(
        "line " + line + ":" + charPositionInLine + " " + msg)
    }
  }

  private def genParser(input: String): EECParser = {
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