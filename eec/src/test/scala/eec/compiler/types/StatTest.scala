package eec
package compiler
package types

import BootstrapTests._
import StatBootstraps._

import org.junit.Test
import org.junit.Assert._

class StatTest {

  @Test def typecheckTrivial() = typecheck(
    "()" -|: "unit: () = ()"
  )

  @Test def typecheckWildcard() = typecheck(
    "forall a b . a -> b -> a" -|: "const x _: A -> B -> A = x"
  )

  @Test def typecheckLinearSig() = typecheck(
    "forall a a# b# . a -> (a# ->. b# +: a#)"
    -|: """ InRproxy _ [r] : A -> (R# ->. L# +: R#) =
              InR[r] """,

    "forall a# . a# +: ()"
    -|: """ foo : L# +: () =
              (InRproxy 0)[()] """,

    "() -> (() ->. ())"
    -|: """ primitive ok _ [_] : () -> (() ->. ()) """,

    "() -> (() ->. ())"
    -|: """ linearFail _ [_] : () -> (() ->. ()) = () """,
  )

  @Test def failLinearSum() = noType(
    """ evaluation [a]: A# ->. () |: () =
          Left () """ // error - `a` is not allowed to be in scope
  )

  @Test def failLinearBang() = noType(
    """ evaluation [a]: A# ->. !() =
          !() """ // error - `a` is not allowed to be in scope
  )

  @Test def failLinearConstant() = noType(
    """ evaluation [a]: A# ->. () =
          0 """, // error - `a` is not allowed to be in scope
    """ evaluation [_]: A# ->. () =
          0 """ // error - `_` is not allowed to be in scope
  )

  @Test def failRecursion() = noType(
    """ fix f: (t -> !t) -> !t =
          let !x = f x in !x """  // error: x in f x is undefined
  )

  @Test def typecheckCompUnification() = typecheck(
    """ eval c : a# -> () = () """        :|- "forall a# . a# -> ()",
    """ foo : () = eval () """            :|- "()",
    """ bar : () = eval ((), ()) """      :|- "()",
    """ baz : () = eval \(c: z#) => c """ :|- "()"
  )

  @Test def failNonCompUnification() = someNoType(
    "eval c : a# -> () = ()", // ok

    "foo: () = eval 0" // error: Integer is not a computation type
  )

  /** linearEval is bad because it tries to project the linear pair more
   *  than once
   */
  @Test def noDuplicateState() = someNoType(
    """ fst[p]: (A#, B#) ->. A# =
          case p of (a, _) =>. a """, // ok

    """ snd[p]: (A#, B#) ->. B# =
          case p of (_, b) =>. b """, // ok

    """ linearEval [pair]: (!A, !A) ->. () =
          let !_ = fst[pair] in
          let !_ = snd[pair] in
          () """ // error: `snd[pair]` has illegal dependency
  )

  @Test def projectTuplesLinear() = typecheck(
    "forall a# b# . (a#, b#) ->. a#"       -|:  """ fst[pair] : (A#, B#) ->. A# =
                                      case pair of (a, _) =>. a """,

    "forall a# b# . (a#, b#) ->. b#"       -|:  """ snd[pair] : (A#, B#) ->. B# =
                                      case pair of (_, b) =>. b """,

    "forall a# b# . (a#, b#) ->. (a#, b#)" -|:  """ linearEval[pair] : (A#, B#) ->. (A#, B#) =
                                      (fst[pair], snd[pair]) """
  )

  /** cantDuplicate is bad because sequentialEval can not have an arg that
   *  depends on a linear variable
   */
  @Test def noDuplicateState2() = someNoType(
    """ duplicateState [e]: A# ->. (A#, A#) =
          (e, e) """, // ok

    """ sequentialEval pair: (!A, !A) -> () =
          case pair of
            (q, r) =>
              let !_ = q in
              let !_ = r in
              () """, // ok

    """ cantDuplicate [e]: !A ->. () =
          sequentialEval (duplicateState e) """, // error: dependency on e
  )

  @Test def typecheckSum() = typecheck(
    "l: () |: q = Left ()"                    :|- "forall a . () |: a",
    "r: m |: () = Right ()"                   :|- "forall a . a |: ()",
    "u: y |: () |: z = Right (Left ())"       :|- "forall a b . a |: () |: b",
    "v: (y |: ()) |: z = Left (Right ())"     :|- "forall a b . (a |: ()) |: b"
  )

  @Test def typecheckLiftBind() = typecheck(
    "forall a a# . (a -> a#) -> !a -> a#" -|: """ lift f x: (a -> b#) -> !a -> b# =
                                                    let !y = x in f y """,

    "forall a b . !a -> (a -> !b) -> !b" -|: """ ma >>= f: !t -> (t -> !u) -> !u =
                                                  lift f ma """,
  )

  @Test def linearNonDuplication() = typecheck(
    "forall a# . a# ->. ()" -|:  """ safeDuplication [a] : A# ->. () =
                          case (a, a) of
                            (q, _) =>. ()
                            (_, r) =>. () """
  )

  @Test def typecheckLinearLambdaEval() = typecheck(
    "forall a# . Void# ->. a#" -|: """ absurdProxy: Void# ->. A# =
                                        \(v: Void#) =>. absurd[v] """,
  )

  @Test def typecheckLambdaApply() = typecheck(
    "forall a . () -> a" -|: """ primitive f a : () -> A """,

    "forall a . () -> a" -|: """ applyTest: () -> A =
                        \(u: ()) => f u """
  )

  @Test def failMatchVariable() =  noType(
    "f e: a -> !a = case e of Left x => !x",  // error: Sum l r is not a
    "g e: a -> !a = case e of (x, _) => !x",  // error: Tuple is not a
    "h e: a -> !a = case e of (_, _) => !e",  // error: Tuple is not a
    "i e: a -> !a = case e of ()     => !e"   // error: Tuple is not a
  )

  @Test def typecheckMaybe() = typecheck(
    "forall a . Maybe a" -|: """ data Maybe a = Just a | Nothing """,
    "forall a . Maybe a -> a |: ()" -|:
      """ maybe_to_or m : Maybe A -> A |: () =
            case m of
              Just a  => Left a;
              Nothing => Right (); """
  )

  @Test def typecheckMaybeC() = typecheck(
    "forall a# . MaybeC a#" -|: """ data MaybeC a# = JustC[a#] | NothingC """,
    "forall a# . MaybeC a# -> a# +: ()" -|:
      """ maybec_to_compsum m: MaybeC A# -> A# +: () =
            case m of
              JustC[a] =>. InL[a];
              NothingC =>. InR[()]; """
  )

  @Test def typecheckMatchSumArbitraryDepth() = typecheck(
    "forall a b c d e . (a |: b) |: c |: d -> (a -> !e) -> (b -> !e) -> (c -> !e) -> (d -> !e) -> !e"
    -|: """ cat4_alt0 e wu xu yu zu: (w |: x) |: y |: z -> (w -> !u) -> (x -> !u) -> (y -> !u) -> (z -> !u) -> !u =
              case e of
                Left  (Left a)  => wu a;
                Left  (Right b) => xu b;
                Right (Left c)  => yu c;
                Right (Right d) => zu d """,

    "forall a b c d e . ((a |: b) |: c) |: d -> (a -> !e) -> (b -> !e) -> (c -> !e) -> (d -> !e) -> !e"
    -|: """ cat4_alt1 e wu xu yu zu: ((w |: x) |: y) |: z -> (w -> !u) -> (x -> !u) -> (y -> !u) -> (z -> !u) -> !u =
              case e of
                Left  (Left  (Left a))  => wu a;
                Left  (Left  (Right b)) => xu b;
                Left  (Right c)         => yu c;
                Right d                 => zu d """,

    "forall a b c d e . a |: b |: c |: d -> (a -> !e) -> (b -> !e) -> (c -> !e) -> (d -> !e) -> !e"
    -|: """ cat4_alt2 e wu xu yu zu: w |: x |: y |: z -> (w -> !u) -> (x -> !u) -> (y -> !u) -> (z -> !u) -> !u =
              case e of
                Left  a                 => wu a;
                Right (Left  b)         => xu b;
                Right (Right (Left c))  => yu c;
                Right (Right (Right d)) => zu d """
  )

  @Test def typecheckLinearMatchSumArbitraryDepth() = typecheck(
    "forall a a# b# c# d# . (a# ->. !a) -> (b# ->. !a) -> (c# ->. !a) -> (d# ->. !a) -> ((a# +: b#) +: c# +: d# ->. !a)"
    -|: """ cat4_alt0 wu xu yu zu [e] : (w# ->. !u) -> (x# ->. !u) -> (y# ->. !u) -> (z# ->. !u) -> ((w# +: x#) +: y# +: z# ->. !u) =
              case e of
                InL[ InL[ a]] =>. wu[a]
                InL[ InR[ b]] =>. xu[b]
                InR[ InL[ c]] =>. yu[c]
                InR[ InR[ d]] =>. zu[d] """,

    "forall a a# b# c# d# . (a# ->. !a) -> (b# ->. !a) -> (c# ->. !a) -> (d# ->. !a) -> (((a# +: b#) +: c#) +: d# ->. !a)"
    -|: """ cat4_alt1 wu xu yu zu [e]: (w# ->. !u) -> (x# ->. !u) -> (y# ->. !u) -> (z# ->. !u) -> (((w# +: x#) +: y#) +: z# ->. !u) =
              case e of
                InL[ InL[ InL[ a]]] =>. wu[a]
                InL[ InL[ InR[ b]]] =>. xu[b]
                InL[ InR[ c]]       =>. yu[c]
                InR[ d]             =>. zu[d] """,

    "forall a a# b# c# d# . (a# ->. !a) -> (b# ->. !a) -> (c# ->. !a) -> (d# ->. !a) -> (a# +: b# +: c# +: d# ->. !a)"
    -|: """ cat4_alt2 wu xu yu zu [e]: (w# ->. !u) -> (x# ->. !u) -> (y# ->. !u) -> (z# ->. !u) -> (w# +: x# +: y# +: z# ->. !u) =
              case e of
                InL[ a]             =>. wu[a]
                InR[ InL[ b]]       =>. xu[b]
                InR[ InR[ InL[ c]]] =>. yu[c]
                InR[ InR[ InR[ d]]] =>. zu[d] """
  )
}