package eec
package compiler
package types

import error.CompilerErrors._
import ast.Trees._
import types.Types._
import types.Typers._
import core.Names._
import core.Contexts._
import Mode._
import util.Convert
import Convert._

import implied ModeOps._
import implied TreeOps._
import implied TypeOps._
import implied NameOps._

object TyperErrors {

  def functorNotMatch(functor: Name, fTpe: Type, functorTyp1: Type, proto1: Type) = {
    val functorTyp1Str  = functorTyp1.show
    val proto1Str       = proto1.show
    val functorNameStr  = functor.show
    CompilerError.UnexpectedType(
      s"Functor definition `$functorNameStr: ${fTpe.show}` does not match args. Expected `$functorTyp1Str` but was `$proto1Str` in type tree.")
  }

  def functionNotMatch(fun: Tree, arg1: Type, argProto: Type) = {
    val argProtoStr = argProto.show
    val arg1Str     = arg1.show
    val funNameStr  = uniqName(fun).show
    CompilerError.UnexpectedType(
      s"Function definition `$funNameStr: ${fun.tpe.show}` does not match args. Expected `$arg1Str` but was `$argProtoStr` in application expr.")
  }

  def linearFunctionNotMatch(fun: Tree, arg1: Type, argProto: Type) = {
    val argProtoStr = argProto.show
    val arg1Str     = arg1.show
    val funNameStr  = uniqName(fun).show
    CompilerError.UnexpectedType(
      s"Linear function definition `$funNameStr: ${fun.tpe.show}` does not match args. Expected `$arg1Str` but was `$argProtoStr` in evaluation expr.")
  }

  def emptyFunctorLinearCtx(fTpe: Type) =
    CompilerError.UnexpectedType(s"Empty Linear Context for Functor Type ${fTpe.show}")

  def emptyFunctor(fTpe: Type) =
    CompilerError.UnexpectedType(s"Empty Functor Type ${fTpe.show}")

  def tupleNoMatch(pt: Type, ptAsTuple: List[_]) = {
    if ptAsTuple.length == 1 then
      typeNotTuple(pt)
    else
      tupleNotMatchLength
  }

  def typeNotTuple(pt: Type) =
    CompilerError.UnexpectedType(s"expected `${pt.show}` but was a Tuple.")

  def tupleNotMatchLength =
    CompilerError.UnexpectedType("Tuple lengths do not match")

  def argsNotMatchLength =
    CompilerError.UnexpectedType("arg lengths do not match")

  def linearUnapplyArgLengthGT1 =
    CompilerError.UnexpectedType("Linear case clause must have a single path.")

  def typecheckFail(typed: Tree)(tpe1: Type, pt: Type) given Mode = {
    CompilerError.UnexpectedType(
      s"Check failed. Type ${typed.tpe.show} != ${pt.show} in ${mode.show}:\n${typed.show}")
  }

  def hkArgsDoNotUnify(functor: Tree, args0: List[Type], args: List[Type]) = {
    val argsOrdered = args0.reverse
    val argsExpect  = argsOrdered.map(_.show).mkString("[", ", ", "]")
    val argsPassed  = args.map(_.show).mkString("[", ", ", "]")
    val hint =
      if args0.length == 0 then {
        val name = (functor.convert: Name).show
        s" Perhaps functor type `$name` is unknown."
      } else {
        ""
      }
    CompilerError.UnexpectedType(
      s"HK args do not match. Expected $argsExpect but got $argsPassed.$hint")
  }

  def typingMissing(tree: Tree) given Mode =
    CompilerError.IllegalState(
      s"Typing not implemented for <${mode.show}, ${tree.show}>")

  def noApplyNonFunctionType =
    CompilerError.IllegalState(s"Can not apply to non function type.")

  def noEvalNonLinearFunctionType =
    CompilerError.IllegalState(s"Can not eval a non linear function type.")

  def tpesNotUnifyTo(tpe: Type) =
    CompilerError.UnexpectedType(s"Types do not unify to ${tpe.show}")

  def noCompArg =
    CompilerError.UnexpectedType(
      "Linear function does not have computational domain.")

  def noLinearCompCodomain =
    CompilerError.UnexpectedType(
      "Linear function does not have computational co-domain.")

  def noTensorCompCodomain(comp1: Tree) = {
    val name = (comp1.convert: Name).show
    val tpe = comp1.tpe.show
    CompilerError.UnexpectedType(
      s"Linear tensor does not have computational second argument. Given `$name: $tpe`")
  }

  def noCompLetContinuation =
    CompilerError.UnexpectedType(
      "Let body does not have computation type.")

  def noTensorLetValue(x: Name, z: Name, value: Tree) =
    CompilerError.UnexpectedType(
      s"Can not infer type of `!${x.show} *: ${x.show} = ${value.show}` as of !_ *: _ type.")

  def noBangLetValue(name: Name, value: Tree) =
    CompilerError.UnexpectedType(
      s"Can not infer type of `!${name.show} = ${value.show}` as of ! type.")

  def notCaseClase(unknown: Tree) =
    CompilerError.IllegalState(
      s"$unknown is not Tree.CaseClause")

  def notLinearCaseClase(unknown: Tree) =
    CompilerError.IllegalState(
      s"$unknown is not Tree.LinearCaseClause")

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

  def nameNotConstructor(name: Name) = {
    CompilerError.UnexpectedType(
      s"${name.show} does not qualify to be a constructor.")
  }

  def illegalStoupDependency(name: Name) =
    CompilerError.UnexpectedType(
      s"Reference to on linear variable `${name.show}` not in scope.")

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