package eec
package compiler
package types

import scala.language.implicitConversions

import error.CompilerErrors._
import ast.Trees._
import TreeOps._
import types.Types._
import types.Typers._
import core.Names._
import core.Contexts._
import Mode._

import implied ModeOps._
import implied TreeOps._
import implied TypeOps._
import implied NameOps._

object TyperErrors {

  def functorNotMatch(functor: Name, fTpe: Type, functorTyp1: Type, proto1: Type) = {
    CompilerError.UnexpectedType(
      s"Functor definition `${functor.show}` of type `${fTpe.show}` does not match args. Expected `${functorTyp1.show}` but was `${proto1.show}` in type tree.")
  }

  def functionNotMatch(fun: Tree, arg1: Type, argProto: Type) = {
    CompilerError.UnexpectedType(
      s"Function definition `${uniqName(fun).show}` of type `${fun.tpe.show}` does not match args. Expected `${arg1.show}` but was `${argProto.show}` in application expr.")
  }

  def linearFunctionNotMatch(fun: Tree, arg1: Type, argProto: Type) = {
    CompilerError.UnexpectedType(
      s"Linear function definition `${uniqName(fun).show}` of type `${fun.tpe.show}` does not match args. Expected `${arg1.show}` but was `${argProto.show}` in evaluation expr.")
  }

  def emptyFunctorLinearCtx(fTpe: Type) =
    CompilerError.UnexpectedType(s"Empty Linear Context for Functor Type ${fTpe.show}")

  def emptyFunctor(name: Name, fTpe: Type) =
    CompilerError.UnexpectedType(s"Tried to unapply empty sum constructor ${name.show} of type ${fTpe.show}.")

  def emptyLinearFunctor(name: Name, fTpe: Type) =
    CompilerError.UnexpectedType(s"Tried to unapply empty linear coproduct constructor ${name.show} of type ${fTpe.show}.")

  def functionInLinearUnapply(name: Name, fTpe: Type) =
    CompilerError.UnexpectedType(
      s"Tried to use sum constructor `${name.show}` of type `${fTpe.show}` as a linear coproduct constructor.")

  def tupleNoMatch(pt: Type, ptAsTuple: List[_]) = {
    if ptAsTuple.length == 1 then
      typeNotTuple(pt)
    else
      tupleNotMatchLength
  }

  def nonExhaustivePatterns(templates: List[Tree]) = {
    val temps = {
      templates
        .map(showPatternTemplate(_))
        .mkString("[", ", ", "]")
    }
    CompilerError.UnexpectedType(
      s"Pattern match will fail on values matching these patterns: $temps")
  }

  def typeNotTuple(pt: Type) =
    CompilerError.UnexpectedType(s"Expected `${pt.show}` but was a Tuple.")

  def tupleNotMatchLength =
    CompilerError.UnexpectedType("Tuple lengths do not match.")

  def sumCtorArgsNotMatchLengthPattern(name: Name, in: Int, out: Int) =
    CompilerError.UnexpectedType(
      s"Constructor `${name.show}` has $in function arguments, but an attempt was made to extract $out in a pattern.")

  def linearUnapplyArgLengthGT1 =
    CompilerError.UnexpectedType("Linear case clause must have a single path.")

  def typecheckFail(typed: Tree)(tpe1: Type, pt: Type) given Mode = {
    CompilerError.UnexpectedType(
      s"Check failed. Type ${typed.tpe.show} != ${pt.show} in ${mode.show}:\n${typed.show}")
  }

  def recursiveData(ctor: Name, data: Name) =
    CompilerError.UnexpectedType(
      s"Recursive type reference in constructor ${ctor.show} of ${data.show}.")

  def unresolvedVariable(ctor: Name, data: Name, name: Name) =
    CompilerError.UnexpectedType(
      s"Unresolved type variable ${name.show} in constructor ${ctor.show} of ${data.show}.")

  def typingMissing(tree: Tree) given Mode =
    CompilerError.IllegalState(
      s"Tree given by `${tree.show}` has no implementation for typing in mode ${mode.show}.")

  def noApplyNonFunctionType =
    CompilerError.IllegalState(s"Can not apply to non function type.")

  def noEvalNonLinearFunctionType =
    CompilerError.IllegalState(s"Can not eval a non linear function type.")

  def tpesNotUnifyTo(tpe: Type) =
    CompilerError.UnexpectedType(s"Types do not unify to ${tpe.show}")

  def noCompArg =
    CompilerError.UnexpectedType(
      "Linear function does not have computational domain.")

  def noCompArgCtor(ctor: Name, data: Name, tpe: Type) =
    CompilerError.UnexpectedType(
      s"Argument of value type ${tpe.show} in linear constructor ${ctor.show} of ${data.show}.")

  def noLinearCompCodomain =
    CompilerError.UnexpectedType(
      "Linear function does not have computational co-domain.")

  def noTensorCompCodomain(comp1: Tree) = {
    val name = (comp1: Name).show
    val tpe = comp1.tpe.show
    CompilerError.UnexpectedType(
      s"Linear tensor does not have computational second argument. Given `$name` of type `$tpe`")
  }

  def noCompLetContinuation =
    CompilerError.UnexpectedType(
      "Let body does not have computation type.")

  def noTensorLetValue(value: Tree) =
    CompilerError.UnexpectedType(
      s"Can not infer type of `${value.show}` as of *: type.")

  def noBangLetValue(value: Tree) =
    CompilerError.UnexpectedType(
      s"Can not infer type of `${value.show}` as of ! type.")

  def notCaseClase(unknown: Tree) =
    CompilerError.IllegalState(
      s"${unknown.show} is not Tree.CaseClause")

  def notGenCtorSig(unknown: Tree) =
    CompilerError.IllegalState(
      s"${unknown.show} is not constructor signature")

  def notLinearCaseClase(unknown: Tree) =
    CompilerError.IllegalState(
      s"${unknown.show} is not Tree.LinearCaseClause")

  def nameInPattAlt(name: Name) =
    CompilerError.IllegalState(
      s"Illegal variable ${name.show} in pattern alternative")

  def declArgsNotMatchType(name: Name) = {
    CompilerError.UnexpectedType(
      s"Function declaration arguments do not match declared type for declaration ${name.show}.")
  }

  def linearArgNotMatchType(name: Name) = {
    CompilerError.UnexpectedType(
      s"No linear context found to bind function declaration argument ${name.show}.")
  }

  def illegalStoupEntry(name: Name, tpe: Type) =
    CompilerError.UnexpectedType(
        s"name `${name.show}` of value type: `${tpe.show}` is not allowed in the linear context.")

  def illegalStoupDependency(name: Name) =
    CompilerError.UnexpectedType(
      s"Reference to linear variable `${name.show}` not in scope.")

  def illegalStoupBang(name: Name) =
    CompilerError.UnexpectedType(
      s"linear variable `${name.show}` can not be in scope when evaluating ! terms.")

  def illegalStoupLinearLambda(name: Name) =
    CompilerError.UnexpectedType(
      s"linear variable `${name.show}` can not be in scope when evaluating linear lambda terms.")

  def illegalStoupValueIdent(z: Name, name: Name, tpe: Type) =
    CompilerError.UnexpectedType(
      s"linear variable `${z.show}` can not be in scope when evaluating variable `${name.show}: ${tpe.show}` of value type.")

  def illegalStoupValueLiteral(name: Name) =
    CompilerError.UnexpectedType(
      s"linear variable `${name.show}` can not be in scope when evaluating a constant literal.")

  def memberSelection given Mode =
    CompilerError.SyntaxError(
      s"Member selections do not exist for ${mode.show}.")
}