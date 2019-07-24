package eec
package compiler
package parsing

import scala.language.implicitConversions

import scala.collection.JavaConverters._
import scala.collection.mutable

import ast._
import core._
import Trees.{Tree, TreeOps}
import Tree._
import TreeOps._
import untyped._
import any._
import Names.{NameOps, Name}
import Name._
import NameOps._
import Contexts.{Id, IdGen, IdReader}
import IdGen._
import Constants._
import core.Constants.Constant._
import Modifiers.{Modifier}
import Modifier._
import MesaParser._
import types.Types.TypeOps._
import error.CompilerErrors._

import org.antlr.v4.runtime._

import delegate TreeOps._
import delegate NameOps._

object Parsers {

  private[parsing] val eecParser =
    genParser `andThen` { _.translationUnit }

  private[parsing] val statParser =
    genParser `andThen` { _.statAsTop }

  private[parsing] val exprParser =
    genParser `andThen` { _.exprAsTop }

  private[parsing] def fromStatAsTop(context: StatAsTopContext)
                                    given IdGen: Lifted[Tree] =
    fromStat(context.stat)

  private[parsing] def fromExprAsTop(context: ExprAsTopContext)
                                    given IdGen: Lifted[Tree] =
    fromExpr(context.expr)

  private[parsing] def fromTranslationUnit(context: TranslationUnitContext)
                                          given IdGen: Lifted[Tree] = {
    import CompilerErrorOps._
    for
      pkgId <- fromPackageInfo(context.packageInfo)
      stats <- lift {
        if defined(context.statSeq) then
          fromStatSeq(context.statSeq)
        else
          EmptyTree
      }
    yield PackageDef(pkgId, stats)(nt)
  }

  private class ParserSyntaxException(msg: String) extends Exception(msg)

  private inline def defined[A <: AnyRef](a: A): Boolean = a `ne` null

  private def freshId() given IdGen: Id = idGen.fresh()

  private val fromSyntax: PartialFunction[Throwable, CompilerError] = {
    case error: ParserSyntaxException =>
      CompilerError.Syntax(error.getMessage)
  }

  def (parser: String => C) toTreeParser[C]
      (toTree: IdReader[C => Lifted[Tree]])
      (input: String) given IdGen: Lifted[Tree] = {
    import CompilerErrorOps._
    for
      context <- parser(input).recover(fromSyntax)
      tree    <- toTree(context)
    yield tree
  }

  private def fromIntegerLiteral(context: LiteralContext): Lifted[Tree] = {
    val txt = context.IntegerLiteral.getText
    if txt.endsWith("l") || txt.endsWith("L") then
      CompilerError.Syntax(s"unexpected Long literal `$txt`")
    else
      Literal(BigIntConstant(BigInt(txt)))(nt)
  }

  private def fromFloatingPointLiteral(
      context: LiteralContext): Lifted[Tree] = {
    val txt = context.FloatingPointLiteral.getText
    if txt.endsWith("f") || txt.endsWith("F") then
      CompilerError.Syntax(s"unexpected Float literal `$txt`")
    else if txt.endsWith("d") || txt.endsWith("D") then
      CompilerError.Syntax(s"unexpected Double literal `$txt`")
    else
      Literal(BigDecConstant(BigDecimal(txt)))(nt)
  }

  private def fromBooleanLiteral(context: LiteralContext): Tree = {
    val bool = context.BooleanLiteral.getText match {
      case "True" => true
      case _      => false
    }
    Literal(BooleanConstant(bool))(nt)
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
    Literal(CharConstant(char))(nt)
  }

  private def fromStringLiteral(context: LiteralContext): Tree = {
    val text    = context.StringLiteral.getText
    val string  = formatString(text)
    Literal(StringConstant(string))(nt)
  }

  private def fromLiteral(context: LiteralContext): Lifted[Tree] = {
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
    Ident(context.OpId.getText.readAs)(freshId(), nt)
  }

  private def fromAlphaId(context: AlphaIdContext) given IdGen: Tree = {
    val name = context.getText.readAs
    Ident(name)(freshId(), nt)
  }

  private def namesToTree(lst: List[Name]) given IdGen: Tree = lst match {
    case n :: Nil   => Ident(n)(freshId(), nt)
    case n :: tail  => Select(namesToTree(tail), n)(freshId(), nt)
    case Nil        => EmptyTree
  }

  private def fromQualId(context: QualIdContext) given IdGen: Tree = {
    val tokens = {
      context
        .id
        .asScala
        .map(_.getText.readAs)
        .toList
    }
    namesToTree(tokens.reverse)
  }

  private def fromStableId(context: StableIdContext) given IdGen: Tree = {
    val alpha = fromAlphaId(context.alphaId)
    if defined(context.id) then
      Select(alpha, context.id.getText.readAs)(freshId(), nt)
    else
      alpha
  }

  private def fromType(context: TypeContext)
                      given IdGen: Lifted[Tree] = {
    if defined(context.func) then
      fromFunc(context.func)
    else if defined(context.lFunc) then
      fromLFunc(context.lFunc)
    else
      fromInfixType(context.infixType)
  }

  private def fromInfixType(context: InfixTypeContext)
                           given IdGen: Lifted[Tree] = {
    if defined(context.functorType) then
      fromFunctorType(context.functorType)
    else
      fromInfixAppliedType(context.infixAppliedType)
  }

  private def fromInfixAppliedType(context: InfixAppliedTypeContext)
                                  given IdGen: Lifted[Tree] = {
    import CompilerErrorOps._
    val name = context.rassocOpId.getText.readAs
    val functor = Ident(name)(freshId(), nt)
    val app = for
      functor1 <- fromFunctorType(context.functorType)
      iat      <- fromInfixType(context.infixType)
    yield InfixApply(functor, functor1, iat)(nt)

    app.flatMap {
      case InfixApply(Ident(other),_,_)
        if other != name && !context.infixType.getText.startsWith("(") =>
          CompilerError.Syntax(
            s"Non associative rhs `${other.show}` to infix type `${name.show}`")

      case other => other
    }
  }

  private def fromLFunc(context: LFuncContext)
                       given IdGen: Lifted[Tree] = {
    import CompilerErrorOps._
    for
      infix <- fromInfixType(context.infixType)
      tpe   <- fromType(context.`type`)
    yield LinearFunction(infix, tpe)(freshId(), nt)
  }

  private def fromFunc(context: FuncContext) given IdGen: Lifted[Tree] = {
    import CompilerErrorOps._
    for
      infix <- fromInfixType(context.infixType)
      tpes <- lift {
        context
          .`type`
          .asScala
          .mapE(fromType)
      }
    yield {
      val head :: rest = tpes.toList.reverse
      val tail = rest.foldLeft(head)((acc, t) => Function(t :: Nil, acc)(freshId(), nt))
      Function(infix :: Nil, tail)(freshId(), nt)
    }
  }

  private def fromFunctorType(context: FunctorTypeContext)
                             given IdGen: Lifted[Tree] = {
    if defined(context.simpleType) then
      fromSimpleType(context.simpleType)
    else
      fromPrefixType(context.prefixType)
  }

  private def fromProductType(context: ProductTypeContext)
                             given IdGen: Lifted[Tree] = {
    import CompilerErrorOps._
    for types <- context.`type`.asScala.mapE(fromType)
    yield parensFromBuffer(types)
  }

  private def fromPrefixType(context: PrefixTypeContext)
                            given IdGen: Lifted[Tree] = {
    if defined(context.Bang) then
      fromBangType(context)
    else
      fromFunctorType(context)
  }

  private def fromFunctorType(context: PrefixTypeContext)
                             given IdGen: Lifted[Tree] = {
    import CompilerErrorOps._
    for simpleTypes <- context.simpleType.asScala.mapE(fromSimpleType) yield {
      val tag  = fromQualId(context.qualId)
      val args = simpleTypes.toList
      Apply(tag, args)(nt)
    }
  }

  private def fromBangType(context: PrefixTypeContext)
                          given IdGen: Lifted[Tree] = {
    import CompilerErrorOps._
    val tag = Ident(Name.BangTag)(freshId(), nt)
    for simpleTypes <- context.simpleType.asScala.mapE(fromSimpleType) yield {
      val args = simpleTypes.toList
      Apply(tag, args)(nt)
    }
  }

  private def fromSimpleType(context: SimpleTypeContext)
                            given IdGen: Lifted[Tree] = {
    if defined(context.qualId) then
      fromQualId(context.qualId)
    else if defined(context.CompId) then
      fromCompId(context)
    else
      fromProductType(context.productType)
  }

  private def fromCompId(context: SimpleTypeContext) given IdGen: Tree =
    Ident(context.CompId.getText.readAs.promoteComp)(freshId(), nt)

  private def fromExpr(context: ExprContext) given IdGen: Lifted[Tree] = {
    if defined(context.lambda) then
      fromLambda(context.lambda)
    else if defined(context.lLambda) then
      fromLLambda(context.lLambda)
    else if defined(context.letExpr) then
      fromLetExpr(context.letExpr)
    else if defined(context.letTensorExpr) then
      fromLetTensorExpr(context.letTensorExpr)
    else if defined(context.caseExpr) then
      fromCaseExpr(context.caseExpr)
    else if defined(context.lCaseExpr) then
      fromLCaseExpr(context.lCaseExpr)
    else if defined(context.expr1) then
      fromExpr1(context.expr1)
    else
      fromExprSeqAsApply(context.expr)
  }

  private def fromExprSeqAsApply(exprs: java.util.List[ExprContext])
                                given IdGen: Lifted[Tree] = {
    import CompilerErrorOps._
    for
      exprs <- exprs.asScala.mapE(fromExpr)
      Seq(expr, arg) = exprs
    yield Apply(expr, arg)(nt)
  }

  private def fromLambda(context: LambdaContext) given IdGen: Lifted[Tree] = {
    import CompilerErrorOps._
    for
      body     <- fromExpr(context.expr)
      bindings <- fromBindings(context.bindings)
    yield Function(bindings, body)(freshId(), nt)
  }

  private def fromLLambda(context: LLambdaContext)
                         given IdGen: Lifted[Tree] = {
    import CompilerErrorOps._
    for
      body    <- fromExpr(context.expr)
      binding <- fromBinding(context.binding)
    yield LinearFunction(binding, body)(freshId(), nt)
  }

  private def fromLetExpr(context: LetExprContext)
                         given IdGen: Lifted[Tree] = {
    import CompilerErrorOps._
    for
      patt  <- fromSimplePattern(context.simplePattern)
      exprs <- context.expr.asScala.mapE(fromExpr)
    yield {
      val value        = exprs(0)
      val continuation = exprs(1)
      Let(patt, value, continuation)(freshId(), nt)
    }
  }

  private def fromLetTensorExpr(context: LetTensorExprContext)
                               given IdGen: Lifted[Tree] = {
    import CompilerErrorOps._
    for
      pattX <- fromSimplePattern(context.simplePattern)
      pattZ <- fromLPattern(context.lPattern)
      exprs <- context.expr.asScala.mapE(fromExpr)
    yield {
      val value        = exprs(0)
      val continuation = exprs(1)
      LetTensor(pattX, pattZ, value, continuation)(freshId(), nt)
    }
  }

  private def fromCaseExpr(context: CaseExprContext)
                          given IdGen: Lifted[Tree] = {
    import CompilerErrorOps._
    for
      selector <- fromExpr(context.expr)
      cases    <- fromCases(context.cases)
    yield CaseExpr(selector, cases)(nt)
  }

  private def fromLCaseExpr(context: LCaseExprContext)
                           given IdGen: Lifted[Tree] = {
    import CompilerErrorOps._
    for
      selector <- fromExpr(context.expr)
      cases    <- fromLCases(context.lCases)
    yield LinearCaseExpr(selector, cases)(nt)
  }

  private def fromIfElse(context: Expr1Context) given IdGen: Lifted[Tree] = {
    import CompilerErrorOps._
    for exprs <- context.expr.asScala.mapE(fromExpr) yield {
      val caseTrue  = CaseClause(litTrue, EmptyTree, exprs(1))(freshId(), nt)
      val caseFalse = CaseClause(wildcardIdent, EmptyTree, exprs(2))(freshId(), nt)
      val selector  = exprs(0)
      CaseExpr(selector, caseTrue :: caseFalse :: Nil)(nt)
    }
  }

  private def fromExpr1(context: Expr1Context) given IdGen: Lifted[Tree] = {
    if defined(context.infixExpr) then
      fromInfixExpr(context.infixExpr)
    else
      fromIfElse(context)
  }

  private def fromInfixApplication(context: InfixExprContext)
                                  given IdGen: Lifted[Tree] = {
    import CompilerErrorOps._
    val id = {
      if defined(context.OpId) then
        Ident(context.OpId.getText.readAs)(freshId(), nt)
      else
        fromAlphaId(context.alphaId)
    }
    for infixes <- context.infixExpr.asScala.mapE(fromInfixExpr) yield {
      val firstApply = Apply(id, infixes(0) :: Nil)(nt)
      Apply(firstApply, infixes(1) :: Nil)(nt)
    }
  }

  private def fromInfixExpr(context: InfixExprContext)
                           given IdGen: Lifted[Tree] = {
    if defined(context.prefixExpr) then
      fromPrefixExpr(context.prefixExpr)
    else if defined(context.tensorExpr) then
      fromTensorExpr(context.tensorExpr)
    else
      fromInfixApplication(context)
  }

  private def wrapBang(tree: Tree): Lifted[Tree] =
    Tree.Bang(tree)(nt)

  private def wrapWhyNot(tree: Tree): Lifted[Tree] =
    Tree.WhyNot(tree)(nt)

  private def fromTensorExpr(context: TensorExprContext)
                            given IdGen: Lifted[Tree] = {
    import CompilerErrorOps._
    for
      x <- fromSimpleExpr(context.simpleExpr)
      z <- fromInfixExpr(context.infixExpr)
    yield Tree.Tensor(x, z)(nt)
  }

  private def fromPrefixExpr(context: PrefixExprContext)
                            given IdGen: Lifted[Tree] = {
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
                            given IdGen: Lifted[Tree] = {
    if defined(context.literal) then
      fromLiteral(context.literal)
    else if defined(context.OpId) then
      fromOpIdExpr(context)
    else if defined(context.stableId) then
      fromStableId(context.stableId)
    else if defined(context.exprsInParens) then
      fromExprsInParens(context.exprsInParens)
    else
      fromEval(context)
  }

  private def fromOpIdExpr(context: SimpleExprContext): Tree =
    Ident(context.OpId.getText.readAs)(Id.empty, nt)

  private def fromEval(context: SimpleExprContext)
                      given IdGen: Lifted[Tree] = {
    import CompilerErrorOps._
    for
      expr <- fromSimpleExpr(context.simpleExpr)
      eval <- fromExpr(context.expr)
    yield Eval(expr, eval)(nt)
  }

  private def fromCases(context: CasesContext)
                       given IdGen: Lifted[Tree] = {
    import CompilerErrorOps._
    for caseClauses <- context.caseClause.asScala.mapE(fromCaseClause)
    yield caseClauses.toList: Tree
  }

  private def fromLCases(context: LCasesContext)
                        given IdGen: Lifted[Tree] = {
    import CompilerErrorOps._
    for caseClauses <- context.lCaseClause.asScala.mapE(fromLCaseClause)
    yield caseClauses.toList: Tree
  }

  private def fromCaseClause(context: CaseClauseContext)
                            given IdGen: Lifted[Tree] = {
    import CompilerErrorOps._
    for
      pat   <- fromPattern(context.pattern)
      guard <- lift {
        if defined(context.guard) then
          fromGuard(context.guard)
        else
          EmptyTree
      }
      body <- fromExpr(context.expr)
    yield CaseClause(pat, guard, body)(freshId(), nt)
  }

  private def fromLCaseClause(context: LCaseClauseContext)
                             given IdGen: Lifted[Tree] = {
    import CompilerErrorOps._
    for
      pat  <- fromLPattern(context.lPattern)
      body <- fromExpr(context.expr)
    yield LinearCaseClause(pat, body)(freshId(), nt)
  }

  private def fromExprsInParens(context: ExprsInParensContext)
                               given IdGen: Lifted[Tree] = {
    import CompilerErrorOps._
    for exprs <- context.expr.asScala.mapE(fromExpr)
    yield parensFromBuffer(exprs)
  }

  private def fromLPattern(context: LPatternContext)
                          given IdGen: Lifted[Tree] = {
    if defined(context.Wildcard) then
      wildcardIdent
    else if defined(context.Varid) then
      fromVaridLPattern(context)
    else if defined(context.literal) then
      fromLiteral(context.literal)
    else if defined(context.Patid) then
      fromLUnapply(context)
    else if defined(context.lPatterns) then
      fromLPatterns(context.lPatterns)
    else
      unit
  }

  private def fromLUnapply(context: LPatternContext)
                          given IdGen: Lifted[Tree] = {
    import CompilerErrorOps._
    val functor = context.Patid.getText.readAs
    for binding <- Option(context.lPattern).mapE(fromLPattern)
    yield Unapply(functor, binding.toList)(nt)
  }

  private def fromLPatterns(context: LPatternsContext)
                           given IdGen: Lifted[Tree] = {
    import CompilerErrorOps._
    for patterns <- context.lPattern.asScala.mapE(fromLPattern)
    yield parensFromBuffer(patterns)
  }

  private def fromVaridLPattern(context: LPatternContext)
                               given IdGen: Tree = {
    val name = context.Varid.getText.readAs
    Ident(name)(freshId(), nt)
  }

  private def fromPattern(context: PatternContext)
                         given IdGen: Lifted[Tree] = {
    import CompilerErrorOps._
    for patterns <- context.pattern1.asScala.mapE(fromPattern1) yield {
      if patterns.size == 1 then
        patterns(0)
      else
        Alternative(patterns.toList)(nt)
    }
  }

  private def fromPattern1(context: Pattern1Context)
                          given IdGen: Lifted[Tree] =
    fromPattern2(context.pattern2)

  private def fromBind(context: Pattern2Context) given IdGen: Lifted[Tree] = {
    import CompilerErrorOps._
    val name = context.Varid.getText.readAs
    if defined(context.pattern3) then {
      for patt3 <- fromPattern3(context.pattern3)
      yield Bind(name, patt3)(nt)
    } else {
      Ident(name)(freshId(), nt)
    }
  }

  private def fromPattern2(context: Pattern2Context)
                          given IdGen: Lifted[Tree] = {
    if defined(context.Varid) then
      fromBind(context)
    else
      fromPattern3(context.pattern3)
  }

  private def fromPattern3(context: Pattern3Context)
                          given IdGen: Lifted[Tree] =
    fromSimplePattern(context.simplePattern)

  private def fromVaridPattern(context: SimplePatternContext)
                              given IdGen: Tree = {
    val name = context.Varid.getText.readAs
    Ident(name)(freshId(), nt)
  }

  private def fromFunctorPattern(context: SimplePatternContext)
                                given IdGen: Lifted[Tree] = {
    import CompilerErrorOps._
    val functor = context.Patid.getText.readAs
    for args <- context.pattern.asScala.mapE(fromPattern)
    yield Unapply(functor, args.toList)(nt)
  }

  private def fromSimplePattern(context: SimplePatternContext)
                               given IdGen: Lifted[Tree] = {
    if defined(context.Wildcard) then
      wildcardIdent
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

  private def fromPatterns(context: PatternsContext)
                          given IdGen: Lifted[Tree] = {
    import CompilerErrorOps._
    for patterns <- context.pattern.asScala.mapE(fromPattern)
    yield parensFromBuffer(patterns)
  }

  private def parensFromBuffer(trees: mutable.Buffer[Tree]): Tree = {
    if trees.size == 0 then
      unit
    else if trees.size == 1 then
      trees(0)
    else
      Parens(trees.toList)(nt)
  }

  private def fromGuard(context: GuardContext) given IdGen: Lifted[Tree] =
    fromInfixExpr(context.infixExpr)

  private def fromBindings(context: BindingsContext)
                          given IdGen: Lifted[Tree] =
    fromBindingsTagged(context.bindingsTagged)

  private def fromBindingsTagged(context: BindingsTaggedContext)
                                given IdGen: Lifted[Tree] = {
    import CompilerErrorOps._
    for bindings <- context.binding.asScala.mapE(fromBinding)
    yield bindings.toList: Tree
  }

  private def fromBinding(context: BindingContext)
                         given IdGen: Lifted[Tree] = {
    import CompilerErrorOps._
    val name: Name  = {
      if defined(context.id) then
        fromId(context.id)
      else
        Name.Wildcard
    }
    for typ <- fromType(context.`type`)
    yield Tagged(name, typ)(nt)
  }

  private def fromDcl(context: DclContext) given IdGen: Lifted[Tree] = {
    if defined(context.primitiveDcl) then
      fromPrimitiveDcl(context.primitiveDcl)
    else if defined(context.lDataDcl) then
      fromLDataDcl(context.lDataDcl)
    else
      fromDataDcl(context.dataDcl)
  }

  private def fromPrimitiveDcl(context: PrimitiveDclContext)
                              given IdGen: Lifted[Tree] = {
    import CompilerErrorOps._
    fromPrimDcl(context.primDecl).map(_.addModifiers(Set(Primitive)))
  }

  private def fromDataDcl(context: DataDclContext)
                         given IdGen: Lifted[Tree] = {
    import CompilerErrorOps._
    fromTypeDcl(context.typeDcl) match {
      case Left(name, args) =>
        for ctors <- fromConstructors(context.constructors)
        yield DataDcl(name, args, ctors)(nt)

      case Right(op, left, right) =>
        for ctors <- fromConstructors(context.constructors)
        yield InfixDataDcl(op, left, right, ctors)(nt)
    }
  }

  private def fromLDataDcl(context: LDataDclContext)
                          given IdGen: Lifted[Tree] = {
    import CompilerErrorOps._
    fromLTypeDcl(context.lTypeDcl) match {
      case Left(name, args) =>
        for ctors <- fromLConstructors(context.lConstructors)
        yield DataDcl(name, args, ctors)(nt)

      case Right(op, left, right) =>
        for ctors <- fromLConstructors(context.lConstructors)
        yield InfixDataDcl(op, left, right, ctors)(nt)
    }
  }

  private def fromLTypeDcl(context: LTypeDclContext)
                          given IdGen: Either[(Name, List[Name]),(Name, Name, Name)] = {
    val args = context.lTpeId.asScala.map(fromLTpeId)
    if defined(context.alphaId) then {
      val name = context.alphaId.getText.readAs
      Left(name, args.toList)
    } else {
      val op = context.rassocOpId.getText.readAs
      Right(op, args(0), args(1))
    }
  }

  private def fromLTpeId(context: LTpeIdContext): Name = {
    if defined(context.CompId) then
      context.CompId.getText.readAs.promoteComp
    else
      context.alphaId.getText.readAs
  }

  private def fromTypeDcl(context: TypeDclContext)
                         given IdGen: Either[(Name, List[Name]),(Name, Name, Name)] = {
    val names = context.alphaId.asScala.map(_.getText.readAs)
    if names.size == 1 then {
      val name = names(0)
      Left(name, Nil)
    } else {
      val names1 = names.toList
      if defined(context.rassocOpId) then {
        val op = context.rassocOpId.getText.readAs
        Right(op, names(0), names(1))
      } else {
        Left(names1.head, names1.tail)
      }
    }
  }

  private def fromConstructors(context: ConstructorsContext)
                              given IdGen: Lifted[List[Tree]] = {
    import CompilerErrorOps._
    context.ctor.asScala.mapE(fromCtor).map(_.toList)
  }

  private def fromLConstructors(context: LConstructorsContext)
                               given IdGen: Lifted[List[Tree]] = {
    import CompilerErrorOps._
    context.lCtor.asScala.mapE(fromLCtor).map(_.toList)
  }

  private def fromCtor(context: CtorContext)
                      given IdGen: Lifted[Tree] = {
   import CompilerErrorOps._
    val name = context.Patid.getText.readAs
    for args <- context.`type`.asScala.mapE(fromType)
    yield CtorSig(name, args.toList)(nt)
  }

  private def fromLCtor(context: LCtorContext)
                       given IdGen: Lifted[Tree] = {
   import CompilerErrorOps._
    val name = context.Patid.getText.readAs
    if defined(context.`type`) then {
      for arg <- fromType(context.`type`)
      yield LinearCtorSig(name, Some(arg))(nt)
    } else {
      LinearCtorSig(name, None)(nt)
    }
  }

  private def fromPrimDcl(context: PrimDeclContext)
                         given IdGen: Lifted[Tree] = {
    import CompilerErrorOps._
    val sig = {
      if defined(context.defSig) then
        fromDefSig(context.defSig)
      else
        fromLSig(context.lSig)
    }
    for typ <- fromType(context.`type`)
    yield DefDef(Set(), sig, typ, EmptyTree)(nt)
  }

  private def fromLSig(context: LSigContext) given IdGen: Tree =
    if defined(context.infixLSig) then fromInfixLSig(context.infixLSig)
    else fromLSigImpl(context)

  private def fromLSigImpl(context: LSigContext) given IdGen: Tree = {
    var name     = (fromAlphaId(context.alphaId): Name)
    val paramids = context.paramName.asScala.map(fromParamName)
    if paramids.size == 1 then
      LinearSig(name, Nil, paramids(0))(freshId(), nt)
    else
      LinearSig(name, paramids.init.toList, paramids.last)(freshId(), nt)
  }

  private def fromInfixLSig(context: InfixLSigContext) given IdGen: Tree = {
    if defined(context.prefixOpLSig) then
      fromPrefixOpLSig(context.prefixOpLSig)
    else if defined(context.OpId) then
      fromInfixOpLSig(context)
    else
      fromInfixAlphaLSig(context)
  }

  private def fromPrefixOpLSig(context: PrefixOpLSigContext) given IdGen: Tree = {
    var args = context.paramName.asScala.map(fromParamName).toList
    val name = context.OpId.getText.readAs
    LinearSig(name, args.init, args.last)(freshId(), nt)
  }

  private def fromInfixOpLSig(context: InfixLSigContext) given IdGen: Tree = {
    var args = context.paramName.asScala.map(fromParamName).toList
    var name = context.OpId.getText.readAs
    LinearSig(name, args.init, args.last)(freshId(), nt)
  }

  private def fromInfixAlphaLSig(context: InfixLSigContext)
                               given IdGen: Tree = {
    var args  = context.paramName.asScala.map(fromParamName).toList
    val name  = (fromAlphaId(context.alphaId): Name)
    LinearSig(name, args.init, args.last)(freshId(), nt)
  }

  private def fromParamName(context: ParamNameContext): Name =
    if defined(context.Wildcard) then Name.Wildcard
    else context.Varid.getText.readAs

  private def fromDef(context: DefContext) given IdGen: Lifted[Tree] =
    fromDefDef(context.defDef)

  private def fromDefDef(context: DefDefContext) given IdGen: Lifted[Tree] = {
    import CompilerErrorOps._
    for
      expr <- fromExpr(context.expr)
      typ  <- fromType(context.`type`)
      sig  = {
        if defined(context.defSig) then
          fromDefSig(context.defSig)
        else
          fromLSig(context.lSig)
      }
    yield DefDef(Set(), sig, typ, expr)(nt)
  }

  private def fromDefSig(context: DefSigContext) given IdGen: Tree = {
    if defined(context.infixDefSig) then
      fromInfixDefSig(context.infixDefSig)
    else
      fromPrefixDefSig(context)
  }

  private def fromPrefixDefSig(context: DefSigContext) given IdGen: Tree = {
    var name      = (fromAlphaId(context.alphaId): Name)
    var paramids  = context.paramName.asScala.map(fromParamName).toList
    DefSig(name, paramids)(freshId(), nt)
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
    var args = context.paramName.asScala.map(fromParamName).toList
    var name = context.OpId.getText.readAs
    DefSig(name, args)(freshId(), nt)
  }

  private def fromInfixAlphaSig(context: InfixDefSigContext)
                               given IdGen: Tree = {
    var args  = context.paramName.asScala.map(fromParamName).toList
    val name  = (fromAlphaId(context.alphaId): Name)
    DefSig(name, args)(freshId(), nt)
  }

  private def fromPrefixOpSig(context: PrefixOpSigContext) given IdGen: Tree = {
    var args = context.paramName.asScala.map(fromParamName).toList
    val name = context.OpId.getText.readAs
    DefSig(name, args)(freshId(), nt)
  }

  private def fromPackageInfo(context: PackageInfoContext) given IdGen: Tree =
    fromQualId(context.qualId)

  private def fromStatSeq(context: StatSeqContext)
                         given IdGen: Lifted[Tree] = {
    import CompilerErrorOps._
    for stats <- context.stat.asScala.mapE(fromStat)
    yield stats.toList: Tree
  }

  private def fromStat(context: StatContext) given IdGen: Lifted[Tree] = {
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
      throw ParserSyntaxException(
        "line " + line + ":" + charPositionInLine + " " + msg)
    }
  }

  private def genParser(input: String): MesaParser = {
    val charStream  = CharStreams.fromString(input)
    val lexer       = MesaLexer(charStream)
    val tokens      = CommonTokenStream(lexer)
    val parser      = MesaParser(tokens)
    lexer.removeErrorListeners
    lexer.addErrorListener(eecErrorListener)
    parser.removeErrorListeners
    parser.addErrorListener(eecErrorListener)
    parser
  }
}