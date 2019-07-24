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
import Name._
import NameOps._
import core.Contexts._
import Mode._

import delegate ModeOps._
import delegate TreeOps._
import delegate TypeOps._
import delegate NameOps._

object TyperErrors {

  def functorNotMatch(functor: Name, fTpe: Type, functorTyp1: Type, proto1: Type) = {
    HKTpeNotMatch("Applied Type", functor, fTpe, functorTyp1, proto1)
  }

  def funcTpeNotMatch(function: Name, fTpe: Type, functorTyp1: Type, proto1: Type) = {
    HKTpeNotMatch("Function", function, fTpe, functorTyp1, proto1)
  }

  def lfuncTpeNotMatch(lfunction: Name, fTpe: Type, functorTyp1: Type, proto1: Type) = {
    HKTpeNotMatch("Linear function", lfunction, fTpe, functorTyp1, proto1)
  }

  def HKTpeNotMatch(kind: String, kindName: Name, fTpe: Type, functorTyp1: Type, proto1: Type) = {
    val nameMsg = kindName.foldEmptyName(kind)(name => s"$kind `${name.define}`")
    CompilerError.UnexpectedType(
      s"$nameMsg of type `${fTpe.show}` does not match args. Expected `${functorTyp1.show}` but was `${proto1.show}` in type tree.")
  }

  def functionNotMatch(fun: Tree, arg1: Type, argProto: Type) = {
    val kind = "Function definition"
    val nameMsg = uniqName(fun).foldEmptyName(kind)(name => s"$kind `${name.define}`")
    CompilerError.UnexpectedType(
      s"$nameMsg of type `${fun.tpe.show}` does not match args. Expected `${arg1.show}` but was `${argProto.show}` in application expr.")
  }

  def linearFunctionNotMatch(fun: Tree, arg1: Type, argProto: Type) = {
    val kind = "Linear function definition"
    val nameMsg = uniqName(fun).foldEmptyName(kind)(name => s"$kind `${name.define}`")
    CompilerError.UnexpectedType(
      s"$nameMsg of type `${fun.tpe.show}` does not match args. Expected `${arg1.show}` but was `${argProto.show}` in evaluation expr.")
  }

  def emptyFunctorLinearCtx(fTpe: Type) =
    CompilerError.UnexpectedType(s"Empty Linear Context for Functor Type ${fTpe.show}")

  def emptyFunctor(name: Name, fTpe: Type) =
    CompilerError.UnexpectedType(s"Tried to unapply empty sum constructor ${name.define} of type ${fTpe.show}.")

  def emptyLinearFunctor(name: Name, fTpe: Type) =
    CompilerError.UnexpectedType(s"Tried to unapply empty linear coproduct constructor ${name.define} of type ${fTpe.show}.")

  def functionInLinearUnapply(name: Name, fTpe: Type) =
    CompilerError.UnexpectedType(
      s"Tried to use sum constructor `${name.define}` of type `${fTpe.show}` as a linear coproduct constructor.")

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
    CompilerError.MissingCase(
      s"Pattern match will fail on values matching these patterns: $temps")
  }

  def typeNotTuple(pt: Type) =
    CompilerError.UnexpectedType(s"Expected `${pt.show}` but was a Tuple.")

  def tupleNotMatchLength =
    CompilerError.UnexpectedType("Tuple lengths do not match.")

  def sumCtorArgsNotMatchLengthPattern(name: Name, in: Int, out: Int) =
    CompilerError.UnexpectedType(
      s"Constructor `${name.define}` has $in function arguments, but an attempt was made to extract $out in a pattern.")

  def linearUnapplyArgLengthGT1 =
    CompilerError.UnexpectedType("Linear case clause must have a single path.")

  def typecheckFail(typed: Tree)(tpe1: Type, pt: Type) given Mode = {
    CompilerError.UnexpectedType(
      s"Check failed. Type ${typed.tpe.show} != ${pt.show} in ${mode.show} for tree given by ${typed.show}")
  }

  def recursiveData(ctor: Name, data: Name) =
    CompilerError.UnexpectedType(
      s"Recursive type reference in constructor ${ctor.define} of ${data.define}.")

  def unresolvedVariable(ctor: Name, data: Name, name: Name) =
    CompilerError.UnknownIdentifier(
      s"Unresolved type variable ${name.define} in constructor ${ctor.define} of ${data.define}.")

  def typingMissing(tree: Tree) given Mode =
    CompilerError.Internal(
      s"No implementation for typing in mode ${mode.show} for tree given by ${tree.show}")

  def noApplyNonFunctionType =
    CompilerError.UnexpectedType(s"Can not apply to non function type.")

  def noEvalNonLinearFunctionType =
    CompilerError.UnexpectedType(s"Can not eval a non-linear function type.")

  def tpesNotUnifyTo(tpe: Type) =
    CompilerError.UnexpectedType(s"Types do not unify to ${tpe.show}")

  def noCompArg =
    CompilerError.UnexpectedType(
      "Linear function does not have computational domain.")

  def noLinearCompCodomain =
    CompilerError.UnexpectedType(
      "Linear function does not have computational co-domain.")

  def noCompArgCtor(ctor: Name, data: Name, tpe: Type) =
    CompilerError.UnexpectedType(
      s"Argument of value type ${tpe.show} in linear constructor ${ctor.define} of ${data.define}.")

  def noTensorCompCodomain(comp1: Tree) = {
    val name = (comp1: Name).define
    val tpe = comp1.tpe.show
    CompilerError.UnexpectedType(
      s"Linear tensor does not have computational second argument. Given `$name` of type `$tpe`")
  }

  def noTensorCompCodomainTpe(tpe: Type) = {
    CompilerError.UnexpectedType(
      s"Linear tensor does not have computational second argument. Given `${tpe.show}`")
  }

  def noCompEvalArg =
    CompilerError.UnexpectedType(
      "Linear evaluation argument does not have computation type.")

  def noCompLetContinuation =
    CompilerError.UnexpectedType(
      "Let body does not have computation type.")

  def noCompLinearCaseContinuation =
    CompilerError.UnexpectedType(
      "Linear case clause body does not have computation type.")

  def noTensorLetValue(value: Tree) =
    CompilerError.UnexpectedType(
      s"Can not infer type as ${TensorTag.define} for tree given by ${value.show}")

  def noBangLetValue(value: Tree) =
    CompilerError.UnexpectedType(
      s"Can not infer type as of ${BangTag.define} type for tree given by ${value.show}")

  def notCaseClause(unknown: Tree) =
    CompilerError.Internal(
      s"not given a case clause by tree ${unknown.show}")

  def notGenCtorSig(unknown: Tree) =
    CompilerError.Internal(
      s"not given a constructor signature by tree ${unknown.show}")

  def notLinearCaseClause(unknown: Tree) =
    CompilerError.Internal(
      s"not given a linear case clause by tree ${unknown.show}")

  def nameInPattAlt(name: Name) =
    CompilerError.NameCollision(
      s"Illegal variable ${name.define} in pattern alternative.")

  def declArgsNotMatchType(name: Name) = {
    CompilerError.UnexpectedType(
      s"Function declaration arguments do not match declared type for declaration ${name.define}.")
  }

  def declArgsInfixNotBinary(name: Name) = {
    CompilerError.UnexpectedType(
      s"Infix function declaration ${name.define} does not have at least two arguments.")
  }

  def declArgsInfixLNotBinary(name: Name) = {
    CompilerError.UnexpectedType(
      s"Infix Linear function declaration ${name.define} does not have at least two arguments.")
  }

  def linearArgNotMatchType(name: Name) = {
    CompilerError.UnexpectedType(
      s"No linear context found to bind linear argument ${name.define}.")
  }

  def memberSelection given Mode =
    CompilerError.Syntax(
      s"Member selections do not exist for ${mode.show}.")

  def illegalStoupDependency(name: Name) =
    CompilerError.UnknownIdentifier(
      s"Reference to linear variable `${name.define}` not in scope.")

  def illegalStoupEntry(name: Name, tpe: Type) =
    CompilerError.LinearScope(
        s"name `${name.define}` of value type `${tpe.show}` is not allowed in the linear context.")

  def illegalStoupBang(name: Name) =
    CompilerError.LinearScope(
      s"linear variable `${name.define}` can not be in scope when evaluating ${BangTag.define} terms.")

  def illegalStoupLinearLambda(name: Name) =
    CompilerError.LinearScope(
      s"linear variable `${name.define}` must not be in scope when introducing a new linear lambda term.")

  def illegalStoupValueIdent(z: Name, name: Name, tpe: Type) =
    CompilerError.LinearScope(
      s"linear variable `${z.define}` can not be in scope when evaluating variable `${name.define}` of type `${tpe.show}`.")

  def illegalStoupValueLiteral(name: Name) =
    CompilerError.LinearScope(
      s"linear variable `${name.define}` can not be in scope when evaluating a constant literal.")

  def illegalStoupLambda(name: Name) =
    CompilerError.NameCollision(
      s"linear variable `${name.define}` would be illegally shadowed by lambda argument whilst in scope.")
}