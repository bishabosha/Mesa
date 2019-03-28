package eec
package compiler
package types

import error.CompilerErrors._
import ast.Trees._
import types.Types._
import types.Typers._
import core.Names._
import core.Contexts._
// import Stoup._
import Mode._
import util.Convert
import Convert._

import implied ModeOps._
import implied TreeOps._
import implied TypeOps._
import implied NameOps._
// import implied StoupOps._

object TyperErrors {

  def functorNotMatchProto(functor: Tree, functorTyp1: Type, proto1: Type) = {
    val functorTyp1Str  = functorTyp1.show
    val proto1Str       = proto1.show
    val functorNameStr  = uniqName(functor).show
    CompilerError.UnexpectedType(
      s"Functor definition `$functorNameStr: ${functor.tpe.show}` does not match args. Expected `$functorTyp1Str` but was `$proto1Str` in application expr.")
  }

  def functionNotMatchProto(fun: Tree, arg1: Type, argProto: Type) = {
    val argProtoStr = argProto.show
    val arg1Str     = arg1.show
    val funNameStr  = uniqName(fun).show
    CompilerError.UnexpectedType(
      s"Function definition `$funNameStr: ${fun.tpe.show}` does not match args. Expected `$arg1Str` but was `$argProtoStr` in application expr.")
  }

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

  def typecheckFail(typed: Tree, pt: Type) given Mode = {
    val treeStr = typed.show
    val modeStr = mode.show
    val ptStr   = pt.show
    val tpeStr  = typed.tpe.show
    CompilerError.UnexpectedType(
      s"Check failed. Type $tpeStr != $ptStr in $modeStr:\n$treeStr")
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

  def tpesNotUnifyTo(tpe: Type) =
    CompilerError.UnexpectedType(s"Types do not unify to ${tpe.show}")

  def noCompCodomain =
    CompilerError.UnexpectedType(
      "Function does not have computational co-domain")

  def noCompLetContinuation =
    CompilerError.UnexpectedType(
      "Let body does not have computation type.")

  def noBangLetValue(name: Name, value: Tree) =
    CompilerError.UnexpectedType(
      s"Can not infer type of `!${name.show} = ${value.show}` as of ! type.")

  def notCaseClase(unknown: Tree) =
    CompilerError.IllegalState(
      s"$unknown is not Tree.CaseClause(_,_,_,_)")

  def nameInPattAlt(name: Name) =
    CompilerError.IllegalState(
      s"Illegal variable ${name.show} in pattern alternative")

  def declArgsNotMatchType(name: Name) = {
    CompilerError.UnexpectedType(
      s"Function declaration arguments do not match declared type for declaration ${name.show}.")
  }

  def nameNotConstructor(name: Name) = {
    CompilerError.UnexpectedType(
      s"${name.show} does not qualify to be a constructor.")
  }

  // def stoupNotEmptyBang(tree: Tree) given Context, Stoup =
  //   CompilerError.UnexpectedType(
  //     s"Can not construct ! for argument `${tree.show}` as stoup is `${stoup.show}`")

  def memberSelection given Mode =
    CompilerError.SyntaxError(
      s"Member selections do not exist for ${mode.show}.")
}