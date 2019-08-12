package eec.compiler.types

import StatBootstraps._

class StatTest {

  @test def typecheckTrivial() = typecheck(
    "()" -|: "unit: () = ()"
  )

  @test def typecheckWildcard() = typecheck(
    "forall a b. a -> b -> a" -|:
    """ const x _: A -> B -> A = x """
  )

  @test def typecheckLinearSig() = typecheck(
    "forall a b# c#. a -> b# ->. c# +: b#" -|:
    """ InRproxy _ [r] : A -> (R# ->. L# +: R#) = InR[r] """,

    "forall a#. a# +: ()" -|:
    """ foo : L# +: () = (InRproxy 0)[()] """,

    "() -> () ->. ()" -|:
    """ primitive ok _ [_] : () -> (() ->. ()) """,

    "() -> () ->. ()" -|:
    """ linearFail _ [_] : () -> (() ->. ()) = () """,
  )

  @test def failLinearSum() = noType(
    """ evaluation [a]: A# ->. () |: () =
          Left () """ // error - `a` is not allowed to be in scope
  )

  @test def failLinearBang() = noType(
    """ evaluation [a]: A# ->. !() =
          !() """ // error - `a` is not allowed to be in scope
  )

  @test def failLinearConstant() = noType(
    """ evaluation [a]: A# ->. () =
          0 """, // error - `a` is not allowed to be in scope
    """ evaluation [_]: A# ->. () =
          0 """ // error - `_` is not allowed to be in scope
  )

  @test def failRecursion() = noType(
    """ fix f: (t -> !t) -> !t =
          let !x = f x in !x """  // error: x in f x is undefined
  )

  @test def typecheckCompUnification() = typecheck(
    "forall a#. a# -> ()" -|:
    """ eval c : a# -> () = () """,

    "()" -|: """ foo: () = eval () """,

    "()" -|: """ bar: () = eval ((), ()) """,

    "()" -|: """ baz: () = eval \(c: z#) => c """
  )

  @test def failNonCompUnification() = someNoType(
    "eval c: a# -> () = ()", // ok

    "foo: () = eval 0" // error: Integer is not a computation type
  )

  @test def fixWithConst() = typecheck(
    "forall a b. a -> b -> a" -|:
    """ const a b : A -> B -> A = a """,

    "!Integer" -|:
    """ res0: !Integer = fix (const 0) """
  )

  @test def projectTuplesLinear() = typecheck(
    "forall a# b#. (a#, b#) ->. a#" -|:
    """ fst1[pair] : (A#, B#) ->. A# =
          case pair of (a, _) =>. a """,

    "forall a# b#. (a#, b#) ->. b#" -|:
    """ snd1[pair] : (A#, B#) ->. B# =
          case pair of (_, b) =>. b """,

    "forall a# b#. (a#, b#) ->. (a#, b#)" -|:
    """ linearEval[pair] : (A#, B#) ->. (A#, B#) =
          (fst1[pair], snd1[pair]) """
  )

  /** cantDuplicate is bad because sequentialEval can not have an arg that
   *  depends on a linear variable
   */
  @test def noDuplicateState2() = someNoType(
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

  @test def typecheckSum() = typecheck(
    "forall a. () |: a" -|:
    "l: () |: q = Left ()",

    "forall a. a |: ()" -|:
    "r: m |: () = Right ()",

    "forall a b. a |: () |: b" -|:
    "u: y |: () |: z = Right (Left ())",

    "forall a b. (a |: ()) |: b" -|:
    "v: (y |: ()) |: z = Left (Right ())"
  )

  @test def typecheckLiftBind() = typecheck(
    "forall a b#. (a -> b#) -> !a -> b#" -|:
    """ lift f x: (a -> b#) -> !a -> b# =
          let !y = x in f y """,

    "forall a b. !a -> (a -> !b) -> !b" -|:
    """ ma >>= f: !t -> (t -> !u) -> !u =
          lift f ma """,
  )

  @test def linearNonDuplication() = typecheck(
    "forall a#. a# ->. ()" -|:
    """ safeDuplication [a] : A# ->. () =
          case (a, a) of
            (q, _) =>. ()
            (_, r) =>. () """
  )

  @test def typecheckLinearLambdaEval() = typecheck(
    "forall a#. Void# ->. a#" -|:
    """ absurdProxy: Void# ->. A# =
          \(v: Void#) =>. absurd[v] """,
  )

  @test def typecheckLambdaApply() = typecheck(
    "forall a. () -> a" -|:
    """ primitive f a : () -> A """,

    "forall a. () -> a" -|:
    """ applyTest: () -> A =
          \(u: ()) => f u """
  )

  @test def failMatchVariable() =  noType(
    "f e: a -> !a = case e of Left x => !x",  // error: Sum l r is not a
    "g e: a -> !a = case e of (x, _) => !x",  // error: Tuple is not a
    "h e: a -> !a = case e of (_, _) => !e",  // error: Tuple is not a
    "i e: a -> !a = case e of ()     => !e"   // error: Tuple is not a
  )

  @test def typecheckChoice() = typecheck(
    "Choice" -|: """ data Choice = Yes | No """
  )

  @test def typecheckProduct1() = typecheck(
    "forall a. Product1 a" -|: """ data Product1 A = Product1 A """
  )

  @test def typecheckNewUnit() = typecheck(
    "Unit" -|: """ data Unit = Unit """ // error: first ctor must have an argument
  )

  @test def failNewVoid() = noParse(
    """ data Empty """      // error: cant define empty types
  )

  @test def typecheckMaybe() = typecheck(
    "forall a. Maybe a" -|:
    """ data Maybe a = Just a | Nothing """,

    "forall a. Maybe a -> a |: ()" -|:
    """ maybe_to_or m : Maybe A -> A |: () =
          case m of
            Just a  => Left a;
            Nothing => Right (); """
  )

  @test def typecheckMaybeC() = typecheck(
    "forall a#. MaybeC a#" -|:
    """ data MaybeC a# = JustC[a#] | NothingC """,

    "forall a#. MaybeC a# -> a# +: ()" -|:
    """ maybec_to_compsum m: MaybeC A# -> A# +: () =
          case m of
            JustC[a] =>. InL[a];
            NothingC =>. InR[()]; """
  )

  @test def typecheckMatchBoolean() = typecheck(
    "Boolean -> ()" -|:
    """ match_bool b: Boolean -> () =
          case b of
            True | _ => () """,

    "Boolean -> ()" -|:
    """ match_bool2 b: Boolean -> () =
          case b of
            True | False => () """,

    "Boolean -> ()" -|:
    """ match_bool3 b: Boolean -> () =
          case b of
            False | _ => () """,

    "Boolean -> ()" -|:
    """ match_bool4 b: Boolean -> () =
          case b of
            _ | False => () """,

    "(Boolean, Boolean) -> ()" -|:
    """ match_bool5 b: (Boolean, Boolean) -> () =
          case b of
            (True, False) | (False, True) | (True, True) | (False, False) => () """,
  )

  @test def typecheckMatchSumArbitraryDepth() = typecheck(
    "forall a b c d e. (a |: b) |: c |: d -> (a -> !e) -> (b -> !e) -> (c -> !e) -> (d -> !e) -> !e" -|:
    """ cat4_alt0 e wu xu yu zu: (w |: x) |: y |: z -> (w -> !u) -> (x -> !u) -> (y -> !u) -> (z -> !u) -> !u =
          case e of
            Left  (Left a)  => wu a;
            Left  (Right b) => xu b;
            Right (Left c)  => yu c;
            Right (Right d) => zu d """,

    "forall a b c d e. ((a |: b) |: c) |: d -> (a -> !e) -> (b -> !e) -> (c -> !e) -> (d -> !e) -> !e" -|:
    """ cat4_alt1 e wu xu yu zu: ((w |: x) |: y) |: z -> (w -> !u) -> (x -> !u) -> (y -> !u) -> (z -> !u) -> !u =
          case e of
            Left  (Left  (Left a))  => wu a;
            Left  (Left  (Right b)) => xu b;
            Left  (Right c)         => yu c;
            Right d                 => zu d """,

    "forall a b c d e. a |: b |: c |: d -> (a -> !e) -> (b -> !e) -> (c -> !e) -> (d -> !e) -> !e" -|:
    """ cat4_alt2 e wu xu yu zu: w |: x |: y |: z -> (w -> !u) -> (x -> !u) -> (y -> !u) -> (z -> !u) -> !u =
          case e of
            Left  a                 => wu a;
            Right (Left  b)         => xu b;
            Right (Right (Left c))  => yu c;
            Right (Right (Right d)) => zu d """
  )

  @test def typecheckLinearMatchSumArbitraryDepth() = typecheck(
    "forall a# b c# d# e#. (a# ->. !b) -> (c# ->. !b) -> (d# ->. !b) -> (e# ->. !b) -> (a# +: c#) +: d# +: e# ->. !b" -|:
    """ cat4_alt0 wu xu yu zu [e] : (w# ->. !u) -> (x# ->. !u) -> (y# ->. !u) -> (z# ->. !u) -> ((w# +: x#) +: y# +: z# ->. !u) =
          case e of
            InL[ InL[ a]] =>. wu[a]
            InL[ InR[ b]] =>. xu[b]
            InR[ InL[ c]] =>. yu[c]
            InR[ InR[ d]] =>. zu[d] """,

    "forall a# b c# d# e#. (a# ->. !b) -> (c# ->. !b) -> (d# ->. !b) -> (e# ->. !b) -> ((a# +: c#) +: d#) +: e# ->. !b" -|:
    """ cat4_alt1 wu xu yu zu [e]: (w# ->. !u) -> (x# ->. !u) -> (y# ->. !u) -> (z# ->. !u) -> (((w# +: x#) +: y#) +: z# ->. !u) =
          case e of
            InL[ InL[ InL[ a]]] =>. wu[a]
            InL[ InL[ InR[ b]]] =>. xu[b]
            InL[ InR[ c]]       =>. yu[c]
            InR[ d]             =>. zu[d] """,

    "forall a# b c# d# e#. (a# ->. !b) -> (c# ->. !b) -> (d# ->. !b) -> (e# ->. !b) -> a# +: c# +: d# +: e# ->. !b" -|:
    """ cat4_alt2 wu xu yu zu [e]: (w# ->. !u) -> (x# ->. !u) -> (y# ->. !u) -> (z# ->. !u) -> (w# +: x# +: y# +: z# ->. !u) =
          case e of
            InL[ a]             =>. wu[a]
            InR[ InL[ b]]       =>. xu[b]
            InR[ InR[ InL[ c]]] =>. yu[c]
            InR[ InR[ InR[ d]]] =>. zu[d] """
  )
}