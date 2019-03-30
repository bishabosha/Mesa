package eec
package compiler
package types

import core.Contexts._
import ast.Trees._
import parsing.EntryPoint._
import error.CompilerErrors._
import error.CompilerErrors.CompilerError._
import Types._
import types.Typers._
import error.CompilerErrors._
import BootstrapTests._
import CompilerErrorOps._

import org.junit.Test
import org.junit.Assert._

class TyperTest {

  val any = Type.WildcardType

  @Test def typecheckInteger() = typecheck(
    "0"   -> "Integer",
    "-0"  -> "Integer"
  )

  @Test def failInteger() = noParse(
    "0l", // error: no Longs
    "0L"  // error: no Longs
  )

  @Test def typecheckDecimal() = typecheck(
    "3.14159265358979323846264338328" -> "Decimal", // PI
    "6.62607004e-34"                  -> "Decimal", // Planck's constant
    "-273.15"                         -> "Decimal"  // 0 degrees Kelvin
  )

  @Test def failDecimal() = noParse(
    "3.14159f", // error: no Floats
    "3.14159F", // error: no Floats
    "3.14159d", // error: no Doubles
    "3.14159D"  // error: no Doubles
  )

  @Test def typecheckBoolean() = typecheck(
    "True"  -> "Boolean",
    "False" -> "Boolean"
  )

  @Test def typecheckChar() = typecheck(
    """'a'"""   -> "Char",
    """'\n'"""  -> "Char"
  )

  @Test def failChar() = noParse(
    "''",   // error: empty char
    "'ab'"  // error: char more than one char
  )

  @Test def typecheckString() = typecheck(
    """"test""""        -> "String",
    "\"\"\"test\"\"\""  -> "String"
  )

  @Test def typecheckProducts() = typecheck(
    "()"            -> "()",
    "(())"          -> "()",
    "((),())"       -> "((), ())",
    "((),(),())"    -> "((), (), ())",
    "((),(),(),())" -> "((), (), (), ())"
  )

  @Test def typecheckTensor() = typecheck(
    "!() |*| ()" -> "!() |*| ()",
  )

  @Test def failTensor() = noType(
    "!0 |*| 0", // error: 0 is not of computation type
  )

  @Test def failTensorParse() = noParse(
    "0 |*| ()", // error: syntax error
  )

  @Test def typecheckCompute() = typecheck(
    "!()"       -> "!()",
    "!((),())"  -> "!((), ())",
    "(!(),!())" -> "(!(), !())"
  )

  @Test def failCompute() = noType(
    """\(c: ! a b) => ()""",  // error: expected types [_] but got [a, b]
    "! () ()"                 // error: expected args [_: _] but got [(): (), (): ()]
  )

  @Test def typecheckIf() = typecheck(
    "if True then () else ()" -> "()",
    """\(a: Boolean) (b: Boolean) => if a then !b else !False""" -> "Boolean -> Boolean -> !Boolean"
  )

  @Test def failIf() = noType(
    "if 0 then () else ()",   // error: non Boolean condition
    "if True then 0 else ()"  // error: disjoint branches
  )

  @Test def typecheckCase() = typecheck(
    """case () of
        _ => ()"""                  -> "()",
    """case () of
        x => x"""                   -> "()",
    """case () of
        c @ x => (c, x)"""          -> "((), ())",
    """case () of
        _ if True => ()"""          -> "()",
    """case () of
        ( ) | () => ()"""           -> "()",
    """case "hello" of
        "" | _ => ()"""             -> "()",
    """case ((), ()) of
        ((), ())  => ()
        _         => ()"""          -> "()",
    """case ((), ()) of
        (_, a) => a"""              -> "()",
    """case ((), ((), ())) of
        (_, (_, a)) => a"""         -> "()",
    """case ((), ((), ())) of
        (a, (_, b)) => (a, b)"""    -> "((), ())",
    """case ((), ((), ())) of
        (_, (_, _)) => ()"""        -> "()",
    """case ((), (), ()) of
        ((), (), ()) => ()"""       -> "()",
    """case ((), (), (), ()) of
        ((), (), (), ()) => ()"""   -> "()",
    """case Left () of
        Left x => x;
        Right _ => ()"""            -> "()",
    """case Right () of
        Right x => x;
        Left _ => ()"""             -> "()",
  )

  @Test def failCase() = noType(
    """case () of
        (a | _) => ()""", // error: name in alternative
    """case () of
        ()  => 1
        ()  => False""", // error: disjoint bodies
    """case () of
        "abc" => ()""", // error: pattern doesn't match selector
    """case ((), ((), ())) of
        (((), ()), ()) => 0""", // error: pattern doesn't match selector
    """case ((), ((), ())) of
        ((_, _), _) => 0""", // error: pattern doesn't match selector
    """case "hello" of
        ""  => 0
        1   => 0""" // error: mismatching cases
  )

  @Test def typecheckLinearCase() = typecheck(
    """| case InR [()] of
        InR [m] -* m
        InL [n] -* ()"""          -> "()",
    """| case InR [InL [()]] of
        InR [InL [m]] -* m"""     -> "()",
    """| case ((), ()) of
        (x, _) -* x"""            -> "()",
    """| case ((),()) of
        (x, ( )) -* x"""          -> "()",
    """| case InR [((), ())] of
        InR [(x, _)] -* x"""      -> "()",
    """| case (InR [()], ()) of
        (InR [x], _) -* x"""      -> "()",
  )

  @Test def failLinearCase() = noType(
    """| case (0, ()) of
        (x, _) -* x""",  // error: x: Integer not allowed in stoup
    """| case ((), ()) of
        (x, y) -* x"""   // error: can't put x and y together in stoup
  )

  @Test def typecheckLambda() = typecheck(
    """\(t: ()) => ()"""                -> "() -> ()",
    """\(t: () -> ()) => ()"""          -> "(() -> ()) -> ()",
    """\(t: ()) (u: ()) => ()"""        -> "() -> () -> ()",
    """\(t: ((), ())) => ()"""          -> "((), ()) -> ()",
    """\(t: ((), (), (), ())) => ()"""  -> "((), (), (), ()) -> ()",
    """\(t: b#) => t"""                 -> "b# -> b#"
  )

  @Test def typecheckApplication() = typecheck(
    """(\(t: ()) (u: ()) (v: ()) => ()) ()"""       -> "() -> () -> ()",
    """(\(t: ()) (u: ()) (v: ()) => ()) () ()"""    -> "() -> ()",
    """(\(t: ()) (u: ()) (v: ()) => ()) () () ()""" -> "()",
    """\(f: () -> ()) => f ()"""                    -> "(() -> ()) -> ()",
    """\(t: () -> ()) => \(c: ()) => t c"""         -> "(() -> ()) -> () -> ()"
  )

  @Test def failApplication() = noType(
    """(\(f: ()) => ()) 0""" // error: expects () not Integer
  )

  @Test def typecheckLinearLambda() = typecheck(
    "| (a: A#) -* a" -> "A# -* A#",
  )

  @Test def failLinearLambda() = noType(
    """| (a: A) -* !a""", // error: a is not of computation type, so cant be in stoup
    """| (b: B#) -* !b""", // error: no dependency on b allowed
    """| (a: A#) -* | (b: B#) -* ()""", // error: rhs is not computational codomain
    """(| (a: ()) -* \(b: ()) => | (c: ()) -* a)""" // error: no dependency on a allowed
  )

  @Test def typecheckEval() = typecheck(
    "(| (a: ()) -* a)[()]" -> "()"
  )

  @Test def typecheckLet() = typecheck(
    "let !x = !() in ()"  -> "()",
    "let !x = !0 in !x"   -> "!Integer",
  )

  @Test def failLet() = noType(
    "let !x = () in ()",  // error: () is not ! type
    "let !x = !0 in 0",   // error: 0 is not of computation type
    "| (u: ()) -* let !_ = !() in u", // error: no dependency on u allowed
  )

  @Test def typecheckLetTensor() = typecheck(
    """let !x |*| y = !() |*| () in
        !x |*| y"""                 -> "!() |*| ()",
  )

  @Test def failLetTensor() = noType(
    """let !x |*| y = () in
        !x |*| y""", // error : () is not of tensor type
  )

  @Test def typecheckBind(): Unit = {
    typecheck(
      """\(ma: !a) (f: a -> !b) => let !a = ma in f a"""      -> "!a -> (a -> !b) -> !b",
      """\(ma: !a) => \(f: a -> !b) => let !a = ma in f a"""  -> "!a -> (a -> !b) -> !b",
      """\(a: !a) (f: a -> !b) => let !a = a in f a"""        -> "!a -> (a -> !b) -> !b")
  }

  def typecheck(seq: (String, String)*): Unit = {
    val (idGen, ctx)    = initialCtx
    implied for IdGen   = idGen
    implied for Context = ctx
    seq.toList.foreach { (f, s) => checkTpe(typeExpr(f)(any), s) }
  }

  def noType(seq: String*): Unit = {
    val (idGen, ctx)    = initialCtx
    implied for IdGen   = idGen
    implied for Context = ctx
    seq.toList.foreach { f => failIfUnparsedOrTypedExpr(f)(any) }
  }

  def noParse(seq: String*): Unit = {
    val (idGen, ctx)    = initialCtx
    implied for IdGen   = idGen
    implied for Context = ctx
    seq.toList.foreach { f => failIfParsed(parseExpr)(f) }
  }
}