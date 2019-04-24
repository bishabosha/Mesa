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
    "A -> B -> A" -|: "const x _: A -> B -> A = x"
  )

  @Test def typecheckLinearSig() = typecheck(
    "A -> (R# ->. L# +: R#)"
    -|: """ InRproxy _ [r] : A -> (R# ->. L# +: R#) =
              InR[r] """,

    "L# +: ()"
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
    """ eval c : a# -> () = () """        :|- "a# -> ()",
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
    "(A#, B#) ->. A#"       -|:  """ fst[pair] : (A#, B#) ->. A# =
                                      case pair of (a, _) =>. a """,

    "(A#, B#) ->. B#"       -|:  """ snd[pair] : (A#, B#) ->. B# =
                                      case pair of (_, b) =>. b """,

    "(A#, B#) ->. (A#, B#)" -|:  """ linearEval[pair] : (A#, B#) ->. (A#, B#) =
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
    "l: () |: q = Left ()"                    :|- "() |: q",
    "r: m |: () = Right ()"                   :|- "m |: ()",
    "u: y |: () |: z = Right (Left ())" :|- "y |: () |: z",
    "v: (y |: ()) |: z = Left (Right ())" :|- "(y |: ()) |: z"
  )

  @Test def typecheckLiftBind() = typecheck(
    "(a -> b#) -> !a -> b#" -|: """ lift f x: (a -> b#) -> !a -> b# =
                                      let !y = x in f y """,

    "!t -> (t -> !u) -> !u" -|: """ ma >>= f: !t -> (t -> !u) -> !u =
                                      lift f ma """,
  )

  @Test def linearNonDuplication() = typecheck(
    "A# ->. ()" -|:  """ safeDuplication [a] : A# ->. () =
                          case (a, a) of
                            (q, _) =>. ()
                            (_, r) =>. () """
  )

  @Test def typecheckLinearLambdaEval() = typecheck(
    "Void# ->. A#" -|: """ absurdProxy: Void# ->. A# =
                            \(v: Void#) =>. absurd[v] """,
  )

  @Test def typecheckLambdaApply() = typecheck(
    "() -> A" -|: """ primitive f a : () -> A """,

    "() -> A" -|: """ applyTest: () -> A =
                        \(u: ()) => f u """
  )

  @Test def failMatchVariable() =  noType(
    "f e: a -> !a = case e of Left x => !x",  // error: Sum l r is not a
    "g e: a -> !a = case e of (x, _) => !x",  // error: Tuple is not a
    "h e: a -> !a = case e of (_, _) => !e",  // error: Tuple is not a
    "i e: a -> !a = case e of ()     => !e"   // error: Tuple is not a
  )

  @Test def typecheckMaybe() = typecheck(
    "Maybe a" -|: """ data Maybe a = Just a | Nothing """,
    "Maybe A -> A |: ()" -|:
      """ maybe_to_or m : Maybe A -> A |: () =
            case m of
              Just a  => Left a;
              Nothing => Right (); """
  )

  @Test def typecheckMaybeC() = typecheck(
    "MaybeC a#" -|: """ data MaybeC a# = JustC[a#] | NothingC """,
    "MaybeC A# -> A# +: ()" -|:
      """ maybec_to_compsum m: MaybeC A# -> A# +: () =
            case m of
              JustC[a] =>. InL[a];
              NothingC =>. InR[()]; """
  )

  @Test def typecheckMatchSumArbitraryDepth() = typecheck(
    "(w |: x) |: y |: z -> (w -> !u) -> (x -> !u) -> (y -> !u) -> (z -> !u) -> !u"
    -|: """ cat4_alt0 e wu xu yu zu: (w |: x) |: y |: z -> (w -> !u) -> (x -> !u) -> (y -> !u) -> (z -> !u) -> !u =
              case e of
                Left  (Left a)  => wu a;
                Left  (Right b) => xu b;
                Right (Left c)  => yu c;
                Right (Right d) => zu d """,

    "((w |: x) |: y) |: z -> (w -> !u) -> (x -> !u) -> (y -> !u) -> (z -> !u) -> !u"
    -|: """ cat4_alt1 e wu xu yu zu: ((w |: x) |: y) |: z -> (w -> !u) -> (x -> !u) -> (y -> !u) -> (z -> !u) -> !u =
              case e of
                Left  (Left  (Left a))  => wu a;
                Left  (Left  (Right b)) => xu b;
                Left  (Right c)         => yu c;
                Right d                 => zu d """,

    "w |: x |: y |: z -> (w -> !u) -> (x -> !u) -> (y -> !u) -> (z -> !u) -> !u"
    -|: """ cat4_alt2 e wu xu yu zu: w |: x |: y |: z -> (w -> !u) -> (x -> !u) -> (y -> !u) -> (z -> !u) -> !u =
              case e of
                Left  a                 => wu a;
                Right (Left  b)         => xu b;
                Right (Right (Left c))  => yu c;
                Right (Right (Right d)) => zu d """
  )

  @Test def typecheckLinearMatchSumArbitraryDepth() = typecheck(
    "(w# ->. !u) -> (x# ->. !u) -> (y# ->. !u) -> (z# ->. !u) -> ((w# +: x#) +: y# +: z# ->. !u)"
    -|: """ cat4_alt0 wu xu yu zu [e] : (w# ->. !u) -> (x# ->. !u) -> (y# ->. !u) -> (z# ->. !u) -> ((w# +: x#) +: y# +: z# ->. !u) =
              case e of
                InL[ InL[ a]] =>. wu[a]
                InL[ InR[ b]] =>. xu[b]
                InR[ InL[ c]] =>. yu[c]
                InR[ InR[ d]] =>. zu[d] """,

    "(w# ->. !u) -> (x# ->. !u) -> (y# ->. !u) -> (z# ->. !u) -> (((w# +: x#) +: y#) +: z# ->. !u)"
    -|: """ cat4_alt1 wu xu yu zu [e]: (w# ->. !u) -> (x# ->. !u) -> (y# ->. !u) -> (z# ->. !u) -> (((w# +: x#) +: y#) +: z# ->. !u) =
              case e of
                InL[ InL[ InL[ a]]] =>. wu[a]
                InL[ InL[ InR[ b]]] =>. xu[b]
                InL[ InR[ c]]       =>. yu[c]
                InR[ d]             =>. zu[d] """,

    "(w# ->. !u) -> (x# ->. !u) -> (y# ->. !u) -> (z# ->. !u) -> (w# +: x# +: y# +: z# ->. !u)"
    -|: """ cat4_alt2 wu xu yu zu [e]: (w# ->. !u) -> (x# ->. !u) -> (y# ->. !u) -> (z# ->. !u) -> (w# +: x# +: y# +: z# ->. !u) =
              case e of
                InL[ a]             =>. wu[a]
                InR[ InL[ b]]       =>. xu[b]
                InR[ InR[ InL[ c]]] =>. yu[c]
                InR[ InR[ InR[ d]]] =>. zu[d] """
  )
}