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

  private[this] val unit: Tree =
    Parens(Nil)(uTpe)

  private[parsing] def fromStatAsTop
      (context: EECParser.StatAsTopContext) given IdGen: Checked[Tree] =
    fromStat(context.stat)

  private[parsing] def fromExprAsTop
      (context: EECParser.ExprAsTopContext) given IdGen: Checked[Tree] =
    fromExpr(context.expr)

  private[parsing] def fromTranslationUnit
      (context: EECParser.TranslationUnitContext) given IdGen: Checked[Tree] = {
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

  private[this] class ParserSyntaxException(msg: String) extends Exception(msg)

  private[this] inline def defined[A <: AnyRef](a: A): Boolean = a `ne` null

  private[this] def freshId() given IdGen: Id = idGen.fresh()

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

  private[this] def fromIntegerLiteral
      (context: EECParser.LiteralContext): Checked[Tree] = {
    val txt = context.IntegerLiteral.getText
    if txt.endsWith("l") || txt.endsWith("L") then
      CompilerError.SyntaxError(s"unexpected Long literal `$txt`")
    else
      Literal(BigIntConstant(BigInt(txt)))(uTpe)
  }

  private[this] def fromFloatingPointLiteral
      (context: EECParser.LiteralContext): Checked[Tree] = {
    val txt = context.FloatingPointLiteral.getText
    if txt.endsWith("f") || txt.endsWith("F") then
      CompilerError.SyntaxError(s"unexpected Float literal `$txt`")
    else if txt.endsWith("d") || txt.endsWith("D") then
      CompilerError.SyntaxError(s"unexpected Double literal `$txt`")
    else
      Literal(BigDecConstant(BigDecimal(txt)))(uTpe)
  }

  private[this] def fromBooleanLiteral
      (context: EECParser.LiteralContext): Tree = {
    val bool = context.BooleanLiteral.getText match {
      case "True" => true
      case _      => false
    }
    Literal(BooleanConstant(bool))(uTpe)
  }

  private[this] def charText(context: EECParser.LiteralContext): String = {
    context.CharacterLiteral
      .getText
      .stripPrefix("'").stripSuffix("'")
  }

  private[this] def formatString(text: String): String = {
    if text startsWith "\"\"\"" then
      text stripPrefix "\"\"\"" stripSuffix "\"\"\""
    else
      text stripPrefix "\"" stripSuffix "\""
  }

  private[this] def fromCharacterLiteral
      (context: EECParser.LiteralContext): Tree = {
    val charStr = charText(context)
    val char    = if charStr.length == 0 then 0 else charStr.charAt(0)
    Literal(CharConstant(char))(uTpe)
  }

  private[this] def fromStringLiteral
      (context: EECParser.LiteralContext): Tree = {
    val text    = context.StringLiteral.getText
    val string  = formatString(text)
    Literal(StringConstant(string))(uTpe)
  }

  private[this] def fromLiteral
      (context: EECParser.LiteralContext): Checked[Tree] = {
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

  private[this] def fromId
      (context: EECParser.IdContext) given IdGen: Tree = {
    if defined(context.alphaId) then
      fromAlphaId(context.alphaId)
    else
      fromOpId(context)
  }

  private[this] def fromOpId
      (context: EECParser.IdContext) given IdGen: Tree = {
    val name = context.OpId.getText.readAs
    Ident(name)(freshId(), uTpe)
  }

  private[this] def fromAlphaId
      (context: EECParser.AlphaIdContext) given IdGen: Tree = {
    val name = context.getText.readAs
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
      context
        .id
        .map(_.getText.readAs)
        .toList
    }
    namesToTree(tokens.reverse)
  }

  private[this] def fromStableId
      (context: EECParser.StableIdContext) given IdGen: Tree = {
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

  private[this] def fromType(context: EECParser.TypeContext)
                            given IdGen: Tree = {
    if defined(context.func) then
      fromFunc(context.func)
    else if defined(context.linearFunc) then
      fromLinearFunc(context.linearFunc)
    else
      fromInfixType(context.infixType)
  }

  private[this] def fromInfixType
      (context: EECParser.InfixTypeContext) given IdGen: Tree = {
    if defined(context.functorType) then
      fromFunctorType(context.functorType)
    else
      fromInfixAppliedType(context.infixAppliedType)
  }

  private[this] def fromInfixAppliedType
      (context: EECParser.InfixAppliedTypeContext) given IdGen: Tree = {
    val functor1 = fromFunctorType(context.functorType)
    val name = context.rassocOpId.getText.readAs
    val iat = fromInfixType(context.infixType)
    val functor = Ident(name)(freshId(), uTpe)
    InfixApply(functor, functor1, iat)(uTpe)
  }

  private[this] def fromLinearFunc
      (context: EECParser.LinearFuncContext) given IdGen: Tree = {
    val infixes = {
      context
        .infixType
        .map(fromInfixType)
    }
    LinearFunction(infixes(0), infixes(1))(freshId(), uTpe)
  }

  private[this] def fromFunc
      (context: EECParser.FuncContext) given IdGen: Tree = {
    val infixes = {
      context
        .infixType
        .map(fromInfixType)
    }
    val head :: rest = infixes.toList.reverse
    rest.foldLeft(head)((acc, t) => Function(List(t), acc)(freshId(), uTpe))
  }

  private[this] def fromFunctorType
      (context: EECParser.FunctorTypeContext) given IdGen: Tree = {
    if defined(context.simpleType) then
      fromSimpleType(context.simpleType)
    else
      fromPrefixType(context.prefixType)
  }

  private[this] def fromProductType
        (context: EECParser.ProductTypeContext) given IdGen: Tree = {
    val types = context.`type`.map(fromType)
    parensFromBuffer(types)
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
    val simpleTypes = context.simpleType.map(fromSimpleType)
    val tag         = fromQualId(context.qualId)
    val args        = simpleTypes.toList
    Apply(tag, args)(uTpe)
  }

  private[this] def fromBangType
        (context: EECParser.PrefixTypeContext) given IdGen: Tree = {
    val simpleTypes = context.simpleType.map(fromSimpleType)
    val tag         = Ident(Name.BangTag)(freshId(), uTpe)
    val args        = simpleTypes.toList
    Apply(tag, args)(uTpe)
  }

  private[this] def fromSimpleType
      (context: EECParser.SimpleTypeContext) given IdGen: Tree = {
    if defined(context.qualId) then
      fromQualId(context.qualId)
    else if defined(context.CompId) then
      fromCompId(context)
    else
      fromProductType(context.productType)
  }

  private[this] def fromCompId
      (context: EECParser.SimpleTypeContext) given IdGen: Tree =
    Ident(context.CompId.getText.readAs.promoteComp)(freshId(), uTpe)

  private[this] def fromExpr(context: EECParser.ExprContext)
                            given IdGen: Checked[Tree] = {
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

  private[this] def fromEval
        (exprContext: EECParser.ExprContext, evalContext: EECParser.EvalContext)
        given IdGen: Checked[Tree] = {
    import CompilerErrorOps._
    for {
      expr <- fromExpr(exprContext)
      eval <- fromExpr(evalContext.expr)
    } yield Eval(expr, eval)(uTpe)
  }

  private[this] def fromExprSeqAsApply
        (exprs: java.util.List[EECParser.ExprContext])
        given IdGen: Checked[Tree] = {
    import CompilerErrorOps._
    for {
      exprs <- exprs.mapE(fromExpr)
      Seq(expr, arg) = exprs
    } yield Apply(expr, arg.convert)(uTpe)
  }

  private[this] def fromLambda
      (context: EECParser.LambdaContext) given IdGen: Checked[Tree] = {
    import CompilerErrorOps._
    for (body <- fromExpr(context.expr)) yield {
      val bindings = fromBindings(context.bindings)
      Function(bindings.convert, body)(freshId(), uTpe)
    }
  }

  private[this] def fromLinearLambda
      (context: EECParser.LinearLambdaContext) given IdGen: Checked[Tree] = {
    import CompilerErrorOps._
    for (body <- fromExpr(context.expr)) yield {
      val binding = fromBinding(context.binding)
      LinearFunction(binding, body)(freshId(), uTpe)
    }
  }

  private[this] def fromLetExpr
      (context: EECParser.LetExprContext) given IdGen: Checked[Tree] = {
    import CompilerErrorOps._
    var name = {
      if defined(context.Varid) then
        context.Varid.getText.readAs
      else
        Wildcard
    }
    for (exprs <- context.expr.mapE(fromExpr)) yield {
      val value         = exprs.get(0)
      val continuation  = exprs.get(1)
      Let(name, value, continuation)(freshId(), uTpe)
    }
  }

  private[this] def fromLetTensorExpr
      (context: EECParser.LetTensorExprContext) given IdGen: Checked[Tree] = {
    var varids = context.Varid.map(_.getText.readAs)
    var x = {
      if defined(context.Wildcard) then
        Wildcard
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

  private[this] def fromCaseExpr
      (context: EECParser.CaseExprContext) given IdGen: Checked[Tree] = {
    import CompilerErrorOps._
    for {
      selector  <- fromExpr(context.expr)
      cases     <- fromCases(context.cases)
    } yield CaseExpr(selector, cases.convert)(uTpe)
  }

  private[this] def fromLinearCaseExpr
      (context: EECParser.LinearCaseExprContext) given IdGen: Checked[Tree] = {
    import CompilerErrorOps._
    for {
      selector  <- fromExpr(context.expr)
      cases     <- fromLinearCases(context.linearCases)
    } yield LinearCaseExpr(selector, cases.convert)(uTpe)
  }

  private[this] def fromIfElse
      (context: EECParser.Expr1Context) given IdGen: Checked[Tree] = {
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

  private[this] def fromExpr1
      (context: EECParser.Expr1Context) given IdGen: Checked[Tree] = {
    if defined(context.infixExpr) then
      fromInfixExpr(context.infixExpr)
    else
      fromIfElse(context)
  }

  private[this] def fromInfixApplication
        (context: EECParser.InfixExprContext) given IdGen: Checked[Tree] = {
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

  private[this] def fromInfixExpr
      (context: EECParser.InfixExprContext) given IdGen: Checked[Tree] = {
    if defined(context.prefixExpr) then
      fromPrefixExpr(context.prefixExpr)
    else if defined(context.tensorExpr) then
      fromTensorExpr(context.tensorExpr)
    else
      fromInfixApplication(context)
  }

  private[this] def wrapBang(tree: Tree): Checked[Tree] =
    Bang(tree)(uTpe)

  private[this] def wrapWhyNot(tree: Tree): Checked[Tree] =
    WhyNot(tree)(uTpe)

  private[this] def fromTensorExpr
      (context: EECParser.TensorExprContext) given IdGen: Checked[Tree] = {
    import CompilerErrorOps._
    for {
      x <- fromSimpleExpr(context.simpleExpr)
      z <- fromInfixExpr(context.infixExpr)
    } yield Tensor(x, z)(uTpe)
  }

  private[this] def fromPrefixExpr
      (context: EECParser.PrefixExprContext) given IdGen: Checked[Tree] = {
    import CompilerErrorOps._
    val simpleExpr = fromSimpleExpr(context.simpleExpr)
    if defined(context.Bang) then
      simpleExpr.map(wrapBang)
    else if defined(context.WhyNot) then
      simpleExpr.map(wrapWhyNot)
    else
      simpleExpr
  }

  private[this] def fromSimpleExpr
      (context: EECParser.SimpleExprContext) given IdGen: Checked[Tree] = {
    if defined(context.literal) then
      fromLiteral(context.literal)
    else if defined(context.stableId) then
      fromStableId(context.stableId)
    else
      fromExprsInParens(context.exprsInParens)
  }

  private[this] def fromCases
      (context: EECParser.CasesContext) given IdGen: Checked[Tree] = {
    import CompilerErrorOps._
    for (caseClauses <- context.caseClause.mapE(fromCaseClause))
    yield caseClauses.toList.convert
  }

  private[this] def fromLinearCases
      (context: EECParser.LinearCasesContext) given IdGen: Checked[Tree] = {
    import CompilerErrorOps._
    for (caseClauses <- context.linearCaseClause.mapE(fromLinearCaseClause))
    yield caseClauses.toList.convert
  }

  private[this] def fromCaseClause
      (context: EECParser.CaseClauseContext) given IdGen: Checked[Tree] = {
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

  private[this] def fromLinearCaseClause
      (context: EECParser.LinearCaseClauseContext) given IdGen: Checked[Tree] = {
    import CompilerErrorOps._
    for {
      pat  <- fromLinearPattern(context.linearPattern)
      body <- fromExpr(context.expr)
    } yield LinearCaseClause(pat, body)(freshId(), uTpe)
  }

  private[this] def fromExprsInParens
      (context: EECParser.ExprsInParensContext) given IdGen: Checked[Tree] = {
    import CompilerErrorOps._
    for (exprs <- context.expr.mapE(fromExpr))
    yield parensFromBuffer(exprs)
  }

  private[this] def fromLinearPattern
      (context: EECParser.LinearPatternContext) given IdGen: Tree = {
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

  private[this] def fromLinearUnapply
      (context: EECParser.LinearPatternContext) given IdGen: Tree = {
    import CompilerErrorOps._
    val functor = context.Patid.getText.readAs
    val binding = fromLinearPattern(context.linearPattern)
    Unapply(functor, List(binding))(uTpe)
  }

  private[this] def fromLinearPatterns
      (context: EECParser.LinearPatternsContext) given IdGen: Tree = {
    val patterns = context.linearPattern.map(fromLinearPattern)
    parensFromBuffer(patterns)
  }

  private[this] def fromVaridLinearPattern
      (context: EECParser.LinearPatternContext) given IdGen: Tree = {
    val name = context.Varid.getText.readAs
    Ident(name)(freshId(), uTpe)
  }

  private[this] def fromPattern
      (context: EECParser.PatternContext) given IdGen: Checked[Tree] = {
    import CompilerErrorOps._
    for (patterns <- context.pattern1.mapE(fromPattern1)) yield {
      if patterns.size == 1 then
        patterns(0)
      else
        Alternative(patterns.toList)(uTpe)
    }
  }

  private[this] def fromPattern1
      (context: EECParser.Pattern1Context) given IdGen: Checked[Tree] =
    fromPattern2(context.pattern2)

  private[this] def fromBind
      (context: EECParser.Pattern2Context) given IdGen: Checked[Tree] = {
    import CompilerErrorOps._
    val name = context.Varid.getText.readAs
    if defined(context.pattern3) then {
      for (patt3 <- fromPattern3(context.pattern3))
      yield Bind(name, patt3)(uTpe)
    } else {
      Ident(name)(freshId(), uTpe)
    }
  }

  private[this] def fromPattern2
      (context: EECParser.Pattern2Context) given IdGen: Checked[Tree] = {
    if defined(context.Varid) then
      fromBind(context)
    else
      fromPattern3(context.pattern3)
  }

  private[this] def fromPattern3
      (context: EECParser.Pattern3Context) given IdGen: Checked[Tree] =
    fromSimplePattern(context.simplePattern)

  private[this] def fromVaridPattern
      (context: EECParser.SimplePatternContext) given IdGen: Tree = {
    val name = context.Varid.getText.readAs
    Ident(name)(freshId(), uTpe)
  }

  private[this] def fromFunctorPattern
      (context: EECParser.SimplePatternContext) given IdGen: Checked[Tree] = {
    import CompilerErrorOps._
    val functor = context.Patid.getText.readAs
    for (args <- context.pattern.mapE(fromPattern))
    yield Unapply(functor, args.toList)(uTpe)
  }

  private[this] def fromSimplePattern
      (context: EECParser.SimplePatternContext) given IdGen: Checked[Tree] = {
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

  private[this] def fromPatterns
      (context: EECParser.PatternsContext) given IdGen: Checked[Tree] = {
    import CompilerErrorOps._
    for (patterns <- context.pattern.mapE(fromPattern))
    yield parensFromBuffer(patterns)
  }

  private[this] def parensFromBuffer
      (trees: mutable.Buffer[Tree]): Tree =
    if trees.size == 1 then trees(0)
    else Parens(trees.toList)(uTpe)

  private[this] def fromGuard
      (context: EECParser.GuardContext) given IdGen: Checked[Tree] =
    fromInfixExpr(context.infixExpr)

  private[this] def fromBindings
      (context: EECParser.BindingsContext) given IdGen: Tree =
    fromBindingsTagged(context.bindingsTagged)

  private[this] def fromBindingsTagged
      (context: EECParser.BindingsTaggedContext) given IdGen: Tree = {
    val bindings = context.binding.map(fromBinding)
    bindings.toList.convert
  }

  private[this] def fromBinding
      (context: EECParser.BindingContext) given IdGen: Tree = {
    val name: Name  = {
      if defined(context.id) then
        fromId(context.id).convert
      else
        Wildcard
    }
    val typ = fromType(context.`type`)
    Tagged(name, typ)(uTpe)
  }

  private[this] def fromDcl(context: EECParser.DclContext) given IdGen: Tree =
    fromPrimitiveDcl(context.primitiveDcl)

  private[this] def fromPrimitiveDcl
      (context: EECParser.PrimitiveDclContext) given IdGen: Tree = {
    fromPrimDcl(context.primDecl).addModifiers(Set(Primitive))
  }

  private[this] def fromPrimDcl(context: EECParser.PrimDeclContext)
                               given IdGen: Tree = {
    val sig = {
      if defined(context.defSig) then
        fromDefSig(context.defSig)
      else
        fromLinearSig(context.linearSig)
    }
    val typ = fromType(context.`type`)
    DefDef(Set(), sig, typ, EmptyTree)(uTpe)
  }

  private[this] def fromLinearSig(context: EECParser.LinearSigContext)
                                 given IdGen: Tree = {
    var name     = (fromAlphaId(context.alphaId).convert: Name)
    val paramids = context.paramName.map(fromParamName)
    if paramids.size == 1 then
      LinearSig(name, Nil, paramids(0))(freshId(), uTpe)
    else
      LinearSig(name, paramids.init.toList, paramids.last)(freshId(), uTpe)
  }

  private[this] def fromParamName(context: EECParser.ParamNameContext): Name =
    if defined(context.Wildcard) then Wildcard
    else context.Varid.getText.readAs

  private[this] def fromDef
      (context: EECParser.DefContext) given IdGen: Checked[Tree] =
    fromDefDef(context.defDef)

  private[this] def fromDefDef
      (context: EECParser.DefDefContext) given IdGen: Checked[Tree] = {
    import CompilerErrorOps._
    for {
      expr    <- fromExpr(context.expr)
      sig  = {
        if defined(context.defSig) then
          fromDefSig(context.defSig)
        else
          fromLinearSig(context.linearSig)
      }
      typ     =  fromType(context.`type`)
    } yield DefDef(Set(), sig, typ, expr)(uTpe)
  }

  private[this] def fromDefSig
      (context: EECParser.DefSigContext) given IdGen: Tree = {
    if defined(context.infixDefSig) then
      fromInfixDefSig(context.infixDefSig)
    else
      fromPrefixDefSig(context)
  }

  private[this] def fromPrefixDefSig
      (context: EECParser.DefSigContext) given IdGen: Tree = {
    var name      = (fromAlphaId(context.alphaId).convert: Name)
    var paramids  = context.paramName.map(fromParamName).toList
    DefSig(name, paramids)(freshId(), uTpe)
  }

  private[this] def fromInfixDefSig
      (context: EECParser.InfixDefSigContext) given IdGen: Tree = {
    if defined(context.prefixOpSig) then
      fromPrefixOpSig(context.prefixOpSig)
    else if defined(context.OpId) then
      fromInfixOpSig(context)
    else
      fromInfixAlphaSig(context)
  }

  private[this] def fromInfixOpSig
      (context: EECParser.InfixDefSigContext) given IdGen: Tree = {
    var args = context.paramName.map(fromParamName).toList
    var name = context.OpId.getText.readAs
    DefSig(name, args)(freshId(), uTpe)
  }

  private[this] def fromInfixAlphaSig
      (context: EECParser.InfixDefSigContext) given IdGen: Tree = {
    var args  = context.paramName.map(fromParamName).toList
    val name  = (fromAlphaId(context.alphaId).convert: Name)
    DefSig(name, args)(freshId(), uTpe)
  }

  private[this] def fromPrefixOpSig
      (context: EECParser.PrefixOpSigContext) given IdGen: Tree = {
    var args = context.paramName.map(fromParamName).toList
    val name = context.OpId.getText.readAs
    DefSig(name, args)(freshId(), uTpe)
  }

  private[this] def fromPackageInfo
      (context: EECParser.PackageInfoContext) given IdGen: Tree =
    fromQualId(context.qualId)

  private[this] def fromStatSeq
      (context: EECParser.StatSeqContext) given IdGen: Checked[Tree] = {
    import CompilerErrorOps._
    for (stats <- context.stat.mapE(fromStat))
      yield stats.toList.convert
  }

  private[this] def fromStat(context: EECParser.StatContext)
                            given IdGen: Checked[Tree] = {
    if defined(context.`def`) then
      fromDef(context.`def`)
    else
      fromDcl(context.dcl)
  }

  private[this] val eecErrorListener: BaseErrorListener = new {
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