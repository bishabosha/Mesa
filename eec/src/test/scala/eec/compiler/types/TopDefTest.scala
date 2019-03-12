package eec
package compiler
package types

import org.junit.Test
import org.junit.Assert._

class TopDefTest {

  import core.Contexts._
  import ast.Trees._
  import parsing._
  import error.CompilerErrors._
  import error.CompilerErrors.CompilerError._
  import Types._
  import types.Typers._
  import error.CompilerErrors._

  val any = types.Types.Type.WildcardType

  @Test def typecheckTrivial(): Unit = {
    passesTypeCheck(
      "unit: () = ()" -> "()")
  }

  @Test def typecheckEithers(): Unit = {
    passesTypeCheck(
      "l: Either () r = Left ()"  -> "Either () r",
      "r: Either l () = Right ()" -> "Either l ()")
  }

  @Test def typecheckLiftBind(): Unit = {
    passesTypeCheck(
      """lift f: (a -> !b) -> !a -> !b =
          \c: !a => let !x = c in f x"""   -> "(a -> ! b) -> ! a -> ! b",
      """ma >>= f: !t -> (t -> !u) -> !u =
          (lift f) ma"""                   -> "! t -> (t -> ! u) -> ! u")
  }

  @Test def patternMatchEither(): Unit = {
    passesTypeCheck(
      """fold e lu ru: Either x y -> (x -> !u) -> (y -> !u) -> !u =
          case e of
            Left  a => lu a;
            Right b => ru b"""
      -> "Either x y -> (x -> ! u) -> (y -> ! u) -> ! u")
  }

  @Test def patternMatchEitherArbitraryDepth(): Unit = {
    passesTypeCheck(
      """fold_ e xu yu zu: (Either (Either x y) z) -> (x -> !u) -> (y -> !u) -> (z -> !u) -> !u =
          case e of
            Left  (Left a) => xu a;
            Left  (Right b) => yu a;
            Right c => zu c"""
      -> "(Either (Either x y) z) -> (x -> !u) -> (y -> !u) -> (z -> !u) -> !u")
  }

  def (str: String) typedAs(as: Type): given Context => Checked[Tree] = {
    import Namers._
    import CompilerErrorOps._
    for {
      exp <- parseStat(str)
      _   <- indexAsExpr(exp)
      tpd <- exp.typedAsExpr(any)
    } yield tpd
  }

  def typed(str: String): given Context => Checked[Tree] = str.typedAs(any)

  def passesTypeCheck(seq: (String, String)*): Unit = {

    import CompilerErrorOps._
    import Namers._
    
    val rootCtx = new RootContext()
    implied for Context = rootCtx

    for {
      _ <-  Context.enterBootstrapped
      _ <-  compiler.preludeDefs.mapE { s =>
              for {
                exp <- parseStat(s)
                _   <- indexAsExpr(exp)
                tpd <- exp.typedAsExpr(any)
              } yield ()
            }
      _ <-  seq.toList.mapE((f, s) => impl(typed(f), s))
    } yield ()

    def impl(parsed: Checked[Tree], checkTpe: String): Unit = {
      import CompilerErrorOps._
      import implied CompilerErrorOps._
      import implied TypeOps._
      parsed.fold
        { err => fail(err.userString) }
        { tpd => assertEquals(checkTpe, tpd.tpe.userString) }
    }
  }

  def failsTypeCheck(seq: String*): Unit = {
    import CompilerErrorOps._
    import Namers._

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
      _ <-  compiler.preludeDefs.mapE { s =>
              for {
                exp <- parseStat(s)
                _   <- indexAsExpr(exp)
                tpd <- exp.typedAsExpr(any)
              } yield ()
            }
      _ <-  seq.foreach { f => impl(typed(f)) }
    } yield ()
  }
}