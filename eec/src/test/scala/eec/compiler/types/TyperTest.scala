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

  val wildcard = types.Types.Type.WildcardType

  @Test def typecheckInteger(): Unit = {
    passesTypeCheck(
      "2".typed   -> "Integer",
      "-10".typed -> "Integer")
  }

  @Test def typecheckDecimal(): Unit = {
    passesTypeCheck(
      "3.14159".typed   -> "Decimal",
      "-2.75e-10".typed -> "Decimal")
  }

  @Test def typecheckBoolean(): Unit = {
    passesTypeCheck(
      "True".typed  -> "Boolean",
      "False".typed -> "Boolean")
  }

  @Test def typecheckChar(): Unit = {
    passesTypeCheck(
      "'a'".typed  -> "Char")
  }

  @Test def typecheckString(): Unit = {
    passesTypeCheck(
      """"test"""".typed        -> "String",
      "\"\"\"test\"\"\"".typed  -> "String")
  }

  @Test def typecheckProducts(): Unit = {
    passesTypeCheck(
      "()".typed          -> "()",
      "(1)".typed         -> "Integer",
      "(1, False)".typed  -> "(Integer, Boolean)")
  }

  @Test def typecheckCompute(): Unit = {
    passesTypeCheck(
      "!1".typed          -> "! Integer",
      "!(1)".typed        -> "! Integer",
      "!(1, False)".typed -> "! (Integer, Boolean)")
  }

  @Test def typecheckIf(): Unit = {
    passesTypeCheck(
      "if True then 1 else 0".typed -> "Integer")
    failsTypeCheck(
      "if 0 then () else ()".typed, // non Boolean condition
      "if True then 1 else 'x'".typed) // disjoint branches
  }

  @Test def typecheckCase(): Unit = {
    passesTypeCheck(
      """case "" of
           _ => 0""".typed          -> "Integer",
      """case "" of
           x @ _ => 0""".typed      -> "Integer",
      """case "" of
           x => 0""".typed          -> "Integer",
      """case "" of
           _ if True => 0""".typed  -> "Integer",
      """case "" of
           "" | "a" => 0""".typed   -> "Integer",
      """case (1, False) of
           (2, True) => 3
           _         => 4""".typed  -> "Integer")
    failsTypeCheck(
      """case 5 of
           (a @ 3 | _) => 1""".typed, // name in alternative
      """case 5 of
           6 => 1
           7 => False""".typed, // disjoint bodies
      """case 8 of
           9     => 10
           "abc" => 11""".typed) // disjoint branches
  }

  @Test def typecheckLambda(): Unit = {
    passesTypeCheck(
      "\\i: Integer => 'x'".typed             -> "Integer -> Char",
      "\\i: Integer -> String => 'x'".typed   -> "(Integer -> String) -> Char",
      "\\i: Integer, s: String => 'x'".typed  -> "Integer -> String -> Char",
      "\\i: (Integer, String) => 'x'".typed   -> "(Integer, String) -> Char",
      "\\i: ! Integer => 'x'".typed           -> "! Integer -> Char")
  }

  @Test def typecheckLet(): Unit = {
    passesTypeCheck(
      "let !x = !2 in (!x, !'a')".typed -> "(! Integer, ! Char)")
  }

  def (str: String) typedAs(as: Type): Checked[Type] = {
    import Types.TypeOps._
    for {
      expr   <- parseExpr(str)
      expr1  <- expr.typedAsExpr(as)
    } yield expr1.tpe
  }

  def (str: String) typed: Checked[Type] = str.typedAs(wildcard)

  def passesTypeCheck(seq: (Checked[Type], String)*): Unit = {
    def impl(parsed: Checked[Type], checkTpe: String): Unit =
      parsed.fold { e =>
        fail(e.userString)
      }{ tpe =>
        assertEquals(checkTpe, tpe.userString)
      }
    seq.foreach { impl(_,_) }
  }

  def failsTypeCheck(seq: Checked[Type]*): Unit = {
    def impl(parsed: Checked[Type]): Unit =
      parsed.fold { e =>
        ()
      }{ tpe =>
        fail(s"typed successfully as ${tpe.userString}")
      }
    seq.foreach { impl }
  }
}