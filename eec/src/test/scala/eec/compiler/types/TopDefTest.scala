package eec
package compiler
package types

import org.junit.Test
import org.junit.Assert._

class TopDefTest {

  import core.Contexts._
  import ast.Trees._
  import parsing.EntryPoint._
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
          \(c: !a) => let !x = c in f x"""   -> "(a -> ! b) -> ! a -> ! b",
      """ma >>= f: !t -> (t -> !u) -> !u =
          (lift f) ma"""                   -> "! t -> (t -> ! u) -> ! u")
  }

  @Test def patternMatchEither(): Unit = {
    passesTypeCheck(
      """cat2 e lu ru: Either x y -> (x -> !u) -> (y -> !u) -> !u =
          case e of
            Left  a => lu a;
            Right b => ru b"""
      -> "Either x y -> (x -> ! u) -> (y -> ! u) -> ! u",
      """catTup e lu ru: Either (x, y) z -> (x -> y -> !u) -> (z -> !u) -> !u =
          case e of
            Left  (a, b)  => lu a b;
            Right c       => ru c"""
      -> "Either (x, y) z -> (x -> y -> ! u) -> (z -> ! u) -> ! u")
  }

  @Test def patternMatchVariable(): Unit = {
    passesTypeCheck(
      """f e: a -> !a =
          case e of
            x => !x"""
      -> "a -> ! a")
    failsTypeCheck(
      """f e: a -> !a = case e of Left x => !x""",
      """g e: a -> !a = case e of (x, _) => !x""",
      """h e: a -> !a = case e of (_, _) => !e""",
      """i e: a -> !a = case e of ()     => !e""")
  }

  @Test def patternMatchEitherArbitraryDepth(): Unit = {
    passesTypeCheck(
      """cat4_alt0 e wu xu yu zu: Either (Either w x) (Either y z) -> (w -> !u) -> (x -> !u) -> (y -> !u) -> (z -> !u) -> !u =
          case e of
            Left  (Left a)  => wu a;
            Left  (Right b) => xu b;
            Right (Left c)  => yu c;
            Right (Right d) => zu d"""
      -> "Either (Either w x) (Either y z) -> (w -> ! u) -> (x -> ! u) -> (y -> ! u) -> (z -> ! u) -> ! u",
      """cat4_alt1 e wu xu yu zu: Either (Either (Either w x) y) z -> (w -> !u) -> (x -> !u) -> (y -> !u) -> (z -> !u) -> !u =
          case e of
            Left  (Left  (Left a))  => wu a;
            Left  (Left  (Right b)) => xu b;
            Left  (Right c)         => yu c;
            Right d                 => zu d"""
      -> "Either (Either (Either w x) y) z -> (w -> ! u) -> (x -> ! u) -> (y -> ! u) -> (z -> ! u) -> ! u",
      """cat4_alt2 e wu xu yu zu: Either w (Either x (Either y z)) -> (w -> !u) -> (x -> !u) -> (y -> !u) -> (z -> !u) -> !u =
          case e of
            Left  a                 => wu a;
            Right (Left  b)         => xu b;
            Right (Right (Left c))  => yu c;
            Right (Right (Right d)) => zu d"""
      -> "Either w (Either x (Either y z)) -> (w -> ! u) -> (x -> ! u) -> (y -> ! u) -> (z -> ! u) -> ! u")
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