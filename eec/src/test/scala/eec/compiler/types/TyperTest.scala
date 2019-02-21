package eec
package compiler
package types

import org.junit.Test
import org.junit.Assert._

class TyperTest {

  import ast.Trees._
  import parsing._
  import error.CompilerErrors._
  import error.CompilerErrors.CompilerError._
  import Types._
  import types.Typers._
  import error.CompilerErrors._

  val any = types.Types.Type.WildcardType

  @Test def typecheckInteger(): Unit = {
    passesTypeCheck(
      "0".typed   -> "Integer",
      "-0".typed  -> "Integer")
    failsTypeCheck(
      "0l".typed, // no Longs
      "0L".typed)  // no Longs
  }

  @Test def typecheckDecimal(): Unit = {
    passesTypeCheck(
      "3.14159265358979323846264338328".typed -> "Decimal", // PI
      "6.62607004e-34".typed                  -> "Decimal", // Planck's constant
      "-273.15".typed                         -> "Decimal") // 0 degrees Kelvin
    failsTypeCheck(
      "3.14159f".typed, // no Floats
      "3.14159F".typed, // no Floats
      "3.14159d".typed, // no Doubles
      "3.14159D".typed)  // no Doubles
  }

  @Test def typecheckBoolean(): Unit = {
    passesTypeCheck(
      "True".typed  -> "Boolean",
      "False".typed -> "Boolean")
  }

  @Test def typecheckChar(): Unit = {
    passesTypeCheck(
      "'a'".typed   -> "Char",
      "'\\n'".typed -> "Char")
    failsTypeCheck(
      "''".typed,
      "'ab'".typed)
  }

  @Test def typecheckString(): Unit = {
    passesTypeCheck(
      """"test"""".typed        -> "String",
      "\"\"\"test\"\"\"".typed  -> "String")
  }

  @Test def typecheckProducts(): Unit = {
    passesTypeCheck(
      "()".typed      -> "()",
      "(())".typed    -> "()",
      "((),())".typed -> "((), ())")
  }

  @Test def typecheckCompute(): Unit = {
    passesTypeCheck(
      "!()".typed       -> "! ()",
      "!((),())".typed  -> "! ((), ())")
  }

  @Test def typecheckIf(): Unit = {
    passesTypeCheck(
      "if True then () else ()".typed -> "()")
    failsTypeCheck(
      "if 0 then () else ()".typed, // non Boolean condition
      "if True then 0 else ()".typed) // disjoint branches
  }

  @Test def typecheckCase(): Unit = {
    passesTypeCheck(
      """case () of
           _ => ()""".typed         -> "()",
      """case () of
           x @ _ => ()""".typed     -> "()",
      """case () of
           x => ()""".typed         -> "()",
      """case () of
           _ if True => ()""".typed -> "()",
      """case () of
           () | () => ()""".typed   -> "()",
      """case ((), ()) of
           ((), ()) => ()
           _        => ()""".typed  -> "()")
    failsTypeCheck(
      """case () of
           (a @ () | _) => ()""".typed, // name in alternative
      """case () of
           () => 1
           () => False""".typed, // disjoint bodies
      """case () of
           "abc" => ()""".typed) // disjoint branch from selector
  }

  @Test def typecheckLambda(): Unit = {
    passesTypeCheck(
      "\\t: () => ()".typed         -> "() -> ()",
      "\\t: () -> () => ()".typed   -> "(() -> ()) -> ()",
      "\\t: (), u: () => ()".typed  -> "() -> () -> ()",
      "\\t: ((), ()) => ()".typed   -> "((), ()) -> ()",
      "\\t: ! () => ()".typed       -> "! () -> ()")
    failsTypeCheck(
      "\\f: () -> Char => ()".typed, // f.tpe is not computation co-domain
      "\\f: () => 0".typed) // lambda tpe is not computation co-domain
  }

  @Test def typecheckApplication(): Unit = {
    passesTypeCheck(
      "(\\t: (), u: (), v: () => ()) ()".typed       -> "() -> () -> ()",
      "(\\t: (), u: (), v: () => ()) () ()".typed    -> "() -> ()",
      "(\\t: (), u: (), v: () => ()) () () ()".typed -> "()",
      "\\f: () -> () => f ()".typed                  -> "(() -> ()) -> ()",
      "\\t: () => ! ()".typed                        -> "() -> ! ()",
      "\\t: () -> () => \\c: () => t c".typed        -> "(() -> ()) -> () -> ()")
    failsTypeCheck(
      "(\\f: () => ()) 0".typed) // expects () not Integer
  }

  @Test def typecheckLet(): Unit = {
    passesTypeCheck(
      "let !x = !() in ()".typed -> "()",
      "let !x = !0 in !x".typed -> "! Integer")
    failsTypeCheck(
      "let !x = () in ()".typed, // () is not ! type
      "let !x = 0 in ()".typed, // 0 is not of ! type
      "let !x = !0 in 0".typed) // 0 is not of computation type
  }

  @Test def typecheckBind(): Unit = {
    passesTypeCheck(
      "\\ma: !a, f: a -> !b => let !a = ma in f a".typed      -> "! a -> (a -> ! b) -> ! b",
      "\\ma: !a => \\f: a -> !b => let !a = ma in f a".typed  -> "! a -> (a -> ! b) -> ! b",
      "\\a: !a, f: a -> !b => let !a = a in f a".typed        -> "! a -> (a -> ! b) -> ! b") // modified to test rebinding
  }

  def (str: String) typedAs(as: Type): Checked[Type] = {
    import Types.TypeOps._
    import core.Contexts._
    import Namers._
    import CompilerErrorOps._
    implicit val rootCtx = new RootContext()
    for {
      expr   <- parseExpr(str)
      _      <- Context.enterBootstrapped
      _      <- indexAsExpr(expr)
      expr1  <- expr.typedAsExpr(as)
    } yield expr1.tpe
  }

  def (str: String) typed: Checked[Type] = str.typedAs(any)

  def passesTypeCheck(seq: (Checked[Type], String)*): Unit = {
    def impl(parsed: Checked[Type], checkTpe: String): Unit = {
      import CompilerErrorOps._
      import implied CompilerErrorOps._
      parsed.fold { e =>
        fail(e.userString)
      }{ tpe =>
        assertEquals(checkTpe, tpe.userString)
      }
    }
    seq.foreach { impl(_,_) }
  }

  def failsTypeCheck(seq: Checked[Type]*): Unit = {
    def impl(parsed: Checked[Type]): Unit = {
      import CompilerErrorOps._
      parsed.fold { e =>
        ()
      }{ tpe =>
        fail(s"typed successfully as ${tpe.userString}")
      }
    }
    seq.foreach { impl }
  }
}