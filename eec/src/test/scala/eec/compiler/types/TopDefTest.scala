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

class TopDefTest {

  val any = Type.WildcardType

  @Test def typecheckTrivial() = typecheck(
    "unit: () = ()" -> "()"
  )

  @Test def typecheckLinearSig() = typecheck(
    """InRproxy [r] : R# -* L# |+| R# = InR [r]"""
  -> "R# -* L# |+| R#",
    """InRproxy2 a [r] : A -> (R# -* L# |+| R#) = InR [r]"""
  -> "A -> (R# -* L# |+| R#)",
    """foo : L# |+| () = InRproxy2 0 [()]"""
  -> "L# |+| ()"
  )

  @Test def failRecursion() = noType(
    """fix f: (t -> !t) -> !t =
        let !x = f x in !x"""  // error: x in f x is undefined
  )

  @Test def typecheckCompUnification() = typecheck(
    """eval c : a# -> () = ()"""        -> "a# -> ()",
    """foo : () = eval ()"""            -> "()",
    """bar : () = eval ((), ())"""      -> "()",
    """baz : () = eval \(c: z#) => c""" -> "()"
  )

  @Test def failNonCompUnification() = someNoType(
    "eval c : a# -> () = ()",
    "foo: () = eval 0" // error: Integer is not a computation type
  )

  @Test def typecheckEither() = typecheck(
    "l: Either () q = Left ()"                    -> "Either () q",
    "r: Either m () = Right ()"                   -> "Either m ()",
    "u: Either y (Either () z) = Right (Left ())" -> "Either y (Either () z)",
    "v: Either (Either y ()) z = Left (Right ())" -> "Either (Either y ()) z"
  )

  @Test def typecheckLiftBind() = typecheck(
    """lift f: (a -> b#) -> !a -> b# =
        \(c: !a) => let !x = c in f x"""
  ->"(a -> b#) -> !a -> b#",
    """ma >>= f: !t -> (t -> !u) -> !u =
        (lift f) ma"""
  ->"!t -> (t -> !u) -> !u"
  )

  @Test def typecheckMatchEither() = typecheck(
    """cat2 e lu ru: Either x y -> (x -> !u) -> (y -> !u) -> !u =
        case e of
          Left  a => lu a;
          Right b => ru b"""
  ->"Either x y -> (x -> !u) -> (y -> !u) -> !u",
    """catTup e lu ru: Either (x, y) z -> (x -> y -> !u) -> (z -> !u) -> !u =
        case e of
          Left  (a, b)  => lu a b;
          Right c       => ru c"""
  ->"Either (x, y) z -> (x -> y -> !u) -> (z -> !u) -> !u"
  )

  @Test def typecheckMatchVariable() = typecheck(
    """f e: a -> !a =
        case e of
          x => !x"""
  ->"a -> !a"
  )

  @Test def typecheckLinearLambdaEval() = typecheck(
    """primitive f [a]: () -* A#"""
  -> "() -* A#",
    """evalTest: () -* A# =
        | (u: ()) -* f[u]"""
  -> "() -* A#"
  )

  @Test def typecheckLambdaApply() = typecheck(
    """primitive f a : () -> A"""
  -> "() -> A",
    """applyTest: () -> A =
        \(u: ()) => f u"""
  -> "() -> A"
  )

  @Test def failMatchVariable() =  noType(
    """f e: a -> !a = case e of Left x => !x""",  // error: Either l r is not a
    """g e: a -> !a = case e of (x, _) => !x""",  // error: Tuple is not a
    """h e: a -> !a = case e of (_, _) => !e""",  // error: Tuple is not a
    """i e: a -> !a = case e of ()     => !e"""   // error: Tuple is not a
  )

  @Test def typecheckMatchEitherArbitraryDepth() = typecheck(
    """cat4_alt0 e wu xu yu zu: Either (Either w x) (Either y z) -> (w -> !u) -> (x -> !u) -> (y -> !u) -> (z -> !u) -> !u =
        case e of
          Left  (Left a)  => wu a;
          Left  (Right b) => xu b;
          Right (Left c)  => yu c;
          Right (Right d) => zu d"""
  ->"Either (Either w x) (Either y z) -> (w -> !u) -> (x -> !u) -> (y -> !u) -> (z -> !u) -> !u",
    """cat4_alt1 e wu xu yu zu: Either (Either (Either w x) y) z -> (w -> !u) -> (x -> !u) -> (y -> !u) -> (z -> !u) -> !u =
        case e of
          Left  (Left  (Left a))  => wu a;
          Left  (Left  (Right b)) => xu b;
          Left  (Right c)         => yu c;
          Right d                 => zu d"""
  ->"Either (Either (Either w x) y) z -> (w -> !u) -> (x -> !u) -> (y -> !u) -> (z -> !u) -> !u",
    """cat4_alt2 e wu xu yu zu: Either w (Either x (Either y z)) -> (w -> !u) -> (x -> !u) -> (y -> !u) -> (z -> !u) -> !u =
        case e of
          Left  a                 => wu a;
          Right (Left  b)         => xu b;
          Right (Right (Left c))  => yu c;
          Right (Right (Right d)) => zu d"""
  ->"Either w (Either x (Either y z)) -> (w -> !u) -> (x -> !u) -> (y -> !u) -> (z -> !u) -> !u"
  )

  private def typecheck(seq: (String, String)*): Unit = {
    initialCtx.flatMap { (idGen, ctx) =>
      implied for IdGen = idGen
      implied for Context = ctx
      seq.toList.mapE((f, s) => checkTpe(typeStat(f)(any), s))
    }
  }

  private def noType(seq: String*): Unit = {
    initialCtx.flatMap { (idGen, ctx) =>
      implied for IdGen = idGen
      implied for Context = ctx
      seq.foreach { f => failIfUnparsedOrTypedStat(f)(any) }
    }
  }

  private def someNoType(seq: String*): Unit = {
    initialCtx.flatMap { (idGen, ctx) =>
      implied for IdGen = idGen
      implied for Context = ctx
      failIfAllTyped(seq.mapE { f => typeStat(f)(any) })
    }
  }
}