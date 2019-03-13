package eec
package compiler
package types

import org.junit.Test
import org.junit.Assert._

class TyperTest {

  import core.Contexts._
  import ast.Trees._
  import parsing.EntryPoint._
  import error.CompilerErrors._
  import error.CompilerErrors.CompilerError._
  import Types._
  import types.Typers._
  import error.CompilerErrors._

  val any = types.Types.Type.WildcardType

  @Test def typecheckInteger(): Unit = {
    passesTypeCheck(
      "0"   -> "Integer",
      "-0"  -> "Integer")
    failsTypeCheck(
      "0l",  // error: no Longs
      "0L")  // error: no Longs
  }

  @Test def typecheckDecimal(): Unit = {
    passesTypeCheck(
      "3.14159265358979323846264338328" -> "Decimal", // PI
      "6.62607004e-34"                  -> "Decimal", // Planck's constant
      "-273.15"                         -> "Decimal") // 0 degrees Kelvin
    failsTypeCheck(
      "3.14159f",  // error: no Floats
      "3.14159F",  // error: no Floats
      "3.14159d",  // error: no Doubles
      "3.14159D")  // error: no Doubles
  }

  @Test def typecheckBoolean(): Unit = {
    passesTypeCheck(
      "True"  -> "Boolean",
      "False" -> "Boolean")
  }

  @Test def typecheckChar(): Unit = {
    passesTypeCheck(
      """'a'"""   -> "Char",
      """'\n'""" -> "Char")
    failsTypeCheck(
      "''",   // error: empty char
      "'ab'") // error: char more than one char
  }

  @Test def typecheckString(): Unit = {
    passesTypeCheck(
      """"test""""        -> "String",
      "\"\"\"test\"\"\""  -> "String")
  }

  @Test def typecheckProducts(): Unit = {
    passesTypeCheck(
      "()"            -> "()",
      "(())"          -> "()",
      "((),())"       -> "((), ())",
      "((),(),())"    -> "((), (), ())",
      "((),(),(),())" -> "((), (), (), ())")
  }

  @Test def typecheckCompute(): Unit = {
    passesTypeCheck(
      "!()"       -> "! ()",
      "!((),())"  -> "! ((), ())",
      "(!(),!())" -> "(! (), ! ())")
    failsTypeCheck(
      """\(c: ! a b) => ()""", // error: expected types [_] but got [a, b]
      "! () ()"                 // error: expected args [_: _] but got [(): (), (): ()]
    )
  }

  @Test def typecheckIf(): Unit = {
    passesTypeCheck(
      "if True then () else ()" -> "()",
      """\(a: Boolean) (b: Boolean) => if a then !b else !False""" -> "Boolean -> Boolean -> ! Boolean")
    failsTypeCheck(
      "if 0 then () else ()",   // error: non Boolean condition
      "if True then 0 else ()") // error: disjoint branches
  }

  @Test def typecheckCase(): Unit = {
    passesTypeCheck(
      """case () of
          _ => ()"""                  -> "()",
      """case () of
          x => x"""                   -> "()",
      """case () of
          c @ x => (c, x)"""          -> "((), ())",
      """case () of
          _ if True => ()"""          -> "()",
      """case () of
          () | () => ()"""            -> "()",
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
          ((), (), (), ()) => ()"""   -> "()")
    failsTypeCheck(
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
          1   => 0""") // error: mismatching cases
  }

  @Test def typecheckLambda(): Unit = {
    passesTypeCheck(
      """\(t: ()) => ()"""                -> "() -> ()",
      """\(t: () -> ()) => ()"""          -> "(() -> ()) -> ()",
      """\(t: ()) (u: ()) => ()"""        -> "() -> () -> ()",
      """\(t: ((), ())) => ()"""          -> "((), ()) -> ()",
      """\(t: ((), (), (), ())) => ()"""  -> "((), (), (), ()) -> ()",
      """\(t: !()) => ()"""               -> "! () -> ()")
    failsTypeCheck(
      """\(f: () -> Char) => ()""", // error: f.tpe is not computation co-domain
      """\(f: ()) => 0""")          // error: lambda tpe is not computation co-domain
  }

  @Test def typecheckApplication(): Unit = {
    passesTypeCheck(
      """(\(t: ()) (u: ()) (v: ()) => ()) ()"""       -> "() -> () -> ()",
      """(\(t: ()) (u: ()) (v: ()) => ()) () ()"""    -> "() -> ()",
      """(\(t: ()) (u: ()) (v: ()) => ()) () () ()""" -> "()",
      """\(f: () -> ()) => f ()"""                    -> "(() -> ()) -> ()",
      """\(t: () -> ()) => \(c: ()) => t c"""         -> "(() -> ()) -> () -> ()")
    failsTypeCheck(
      """(\(f: ()) => ()) 0""") // error: expects () not Integer
  }

  @Test def typecheckLet(): Unit = {
    passesTypeCheck(
      "let !x = !() in ()"  -> "()",
      "let !x = !0 in !x"   -> "! Integer")
    failsTypeCheck(
      "let !x = () in ()", // error: () is not ! type
      "let !x = 0 in ()",  // error: 0 is not of ! type
      "let !x = !0 in 0")  // error: 0 is not of computation type
  }

  @Test def typecheckBind(): Unit = {
    passesTypeCheck(
      """\(ma: !a) (f: a -> !b) => let !a = ma in f a"""      -> "! a -> (a -> ! b) -> ! b",
      """\(ma: !a) => \(f: a -> !b) => let !a = ma in f a"""  -> "! a -> (a -> ! b) -> ! b",
      """\(a: !a) (f: a -> !b) => let !a = a in f a"""        -> "! a -> (a -> ! b) -> ! b") // TODO - proof that let allows to rebind names to new variable
  }

  def (str: String) typedAs(as: Type) given Context: Checked[Tree] = {
    import Namers._
    import CompilerErrorOps._
    for {
      expr   <- parseExpr(str)
      _      <- indexAsExpr(expr)
      expr1  <- expr.typedAsExpr(as)
    } yield expr1
  }

  def (str: String) typed given Context: Checked[Tree] = str.typedAs(any)

  def passesTypeCheck(seq: (String, String)*): Unit = {
    import Namers._
    import CompilerErrorOps._

    def impl(parsed: Checked[Tree], checkTpe: String): Unit = {
      import CompilerErrorOps._
      import implied CompilerErrorOps._
      import implied TypeOps._
      parsed.fold
        { err => fail(err.userString) }
        { tpd => assertEquals(checkTpe, tpd.tpe.userString) }
    }

    val rootCtx = new RootContext()
    implied for Context = rootCtx

    for {
      _ <-  Context.enterBootstrapped
      _ <-  seq.toList.mapE((f, s) => impl(typed(f), s))
    } yield ()
  }

  def failsTypeCheck(seq: String*): Unit = {
    import Namers._
    import CompilerErrorOps._

    def impl(parsed: Checked[Tree]): Unit = {
      import CompilerErrorOps._
      import implied TreeOps._
      import implied TypeOps._
      parsed.fold
        { err => () }
        { tpd => fail(
          s"Typed successfully as ${tpd.tpe.userString} for expr:\n${tpd.userString}")
        }
    }

    val rootCtx = new RootContext()
    implied for Context = rootCtx

    for {
      _ <-  Context.enterBootstrapped
      _ <-  seq.toList.mapE(f => impl(typed(f)))
    } yield ()
  }
}