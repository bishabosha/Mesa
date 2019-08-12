package eec.compiler.types

import ExprBootstraps._

import org.junit.{ Test => test }

class ExprTest {

  @test def typecheckInteger() = typecheck(
    "Integer" -|: "0",
    "Integer" -|: "-0"
  )

  @test def failInteger() = noParse(
    "0l", // error: no Longs
    "0L"  // error: no Longs
  )

  @test def typecheckDecimal() = typecheck(
    "Decimal"
    -|: "3.14159265358979323846264338328", // PI

    "Decimal" -|: "6.62607004e-34", // Planck's constant
    "Decimal" -|: "-273.15"         // 0 degrees Kelvin
  )

  @test def failDecimal() = noParse(
    "3.14159f", // error: no Floats
    "3.14159F", // error: no Floats
    "3.14159d", // error: no Doubles
    "3.14159D"  // error: no Doubles
  )

  @test def typecheckBoolean() = typecheck(
    "Boolean" -|: "True",
    "Boolean" -|: "False"
  )

  @test def typecheckChar() = typecheck(
    "Char" -|: """ 'a' """,
    "Char" -|: """ '\n' """
  )

  @test def failChar() = noParse(
    "''",   // error: empty char
    "'ab'"  // error: char more than one char
  )

  @test def typecheckString() = typecheck(
    "String" -|: """ "test" """,
    "String" -|: "\"\"\"test\"\"\""
  )

  @test def typecheckProducts() = typecheck(
    "()"           -|: "()",
    "()"           -|: "(())",
    "((), ())"     -|: "((),())",
    "((), (), ())" -|: "((),(),())",
  )

  @test def typecheckTensor() = typecheck(
    "!() *: ()" -|: "!() *: ()",
  )

  @test def failTensor() = noType(
    "!0 *: 0", // error: 0 is not of computation type
  )

  @test def failTensorParse() = noParse(
    "0 *: ()", // error: syntax error, expecting !
  )

  @test def typecheckCompute() = typecheck(
    "!()"        -|: "!()",
    "!((), ())"  -|: "!((),())",
    "(!(), !())" -|: "(!(),!())",
  )

  @test def failCompute() = noType(
    """ \(_: ! a b) => () """, // error: expected types [_] but got [a, b]
    """ ! () () """, // error: expected args [_: _] but got [(): (), (): ()]
    """ \(x: A#) =>. !x """, // error: `!t` can't depend on linear variable
  )

  @test def typecheckIf() = typecheck(
    "()" -|: "if True then () else ()"
  )

  @test def failIf() = noType(
    "if 0 then () else ()",   // error: non Boolean condition
    "if True then 0 else ()"  // error: disjoint branches
  )

  @test def typecheckCase() = typecheck(
    "()" -|:
    """ case () of _ => () """,

    "()" -|:
    """ case () of x => x """,

    "((), ())" -|:
    """ case () of
          c @ x => (c, x) """,

    "()" -|:
    """ case () of
              _ if True => () """,

    "()" -|:
    """ case () of
              ( ) | () => () """,

    "()" -|:
    """ case ((), ()) of
              (_, a) => a """,

    "()" -|:
    """ case ((), ((), ())) of
              (_, (_, a)) => a """,

    "((), ())" -|:
    """ case ((), ((), ())) of
          (a, (_, b)) => (a, b) """,

    "()" -|:
    """ case ((), (), ()) of
          (_, _, _) => () """,

    "()" -|:
    """ case Left () of
          Left  x => x;
          Right _ => () """,

    "()" -|:
    """ case Left () of
          (Left _) | (Right _) => () """,

    "()" -|:
    """ case (0,0) of
          (1,2) | (_,_) => () """,

    "()" -|:
    """ case True of
          True | _ => () """,

    "()" -|:
    """ case Right 0 of
          (Right 0) | (Left _) | (Right _) => () """,

    "()" -|:
    """ case Left True of
          (Left False) | (Right _) | (Left _) => () """,

    "()" -|:
    """ case Right () of
          Right x => x;
          Left  _ => () """,

    "()" -|:
    """ case False of
          True => ();
          _    => () """,

    "()" -|:
    """ case True of
          False => ();
          _     => () """,
  )

  @test def failCase() = noType(
    """ case Left () of
         Left  x => x """, // error: missing [Right _]

    """ case Right () of
          l @ Left _ => ();
          Left  _ => () """, // error: missing [Right _]

    """ case (0,0) of
          (1,2) | (3,_) => () """, // error: missing [(_: Integer, _: Integer)]

    """ case Right () of
         Right  x => x """, // error: missing [Left _]

    """ case () of
         a | _ => () """, // error: name in alternative

    """ case () of
        _ if False => () """, // error: missing [()] - guard is not idempotent

    """ case () of
          () => 1
          () => False """, // error: disjoint bodies

    """ case () of
          "" => () """, // error: pattern has different type to selector

    """ case ((), ((), ())) of
          (((), ()), ()) => 0 """, // error: pattern has different type to selector

    """ case ((), ((), ())) of
          ((_, _), _) => 0 """, // error: pattern has different type to selector
  )

  @test def typecheckLinearCase() = typecheck(
    "()" -|:
    """ case ((), ()) of
          (x, _) =>. x """,

    "()" -|:
    """ case ((),()) of
          (x, ( )) =>. x """,

    "()" -|:
    """ case InR [((), ())] of
          InR[(x, _)] =>. x
          InL[u]      =>. () """,

    "()" -|:
    """ case (InR [()], ()) of
          (InR[x], _) =>. x
          (InL[u], _) =>. () """,

    "()" -|:
    """ case InL [()] of
          InL[n] =>. n
          InR[u] =>. () """,

    "()" -|:
    """ case 0 of
          0 =>. ()
          _ =>. () """,
  )

  @test def failLinearCase() = noType(
    """ case InL [()] of
          InL[n] =>. n""", // error: missing [InR _]

    """ case InR [()] of
          InR[n] =>. n""", // error: missing [InL _]

    """ case (0, ()) of
          (x, _) =>. x """,    // error: `x: Integer` not allowed in stoup

    """ case ((), ()) of
          (x, y) =>. x """,    // error: can't put x and y together in stoup

    """ case () of
          () =>. 0 """,    // error: 0 is not computation type
  )

  @test def typecheckLambda() = typecheck(
    """ \(t: ()) => () """                :|- "() -> ()",
    """ \(_: ()) => () """                :|- "() -> ()",
    """ \(t: A) => () """                 :|- "forall a. a -> ()",
    """ \(_: A) => () """                 :|- "forall a. a -> ()",
    """ \(t: ()) => t """                 :|- "() -> ()",
    """ \(_: () -> ()) => () """          :|- "(() -> ()) -> ()",
    """ \(_: () ->. ()) => () """         :|- "(() ->. ()) -> ()",
    """ \(_: ()) (_: ()) => () """        :|- "() -> () -> ()",
    """ \(a: ()) (b: ()) => (a,b) """     :|- "() -> () -> ((), ())",
  )
  
  @test def failLambda() = noType(
    """ \(f: () ->. (() ->. ())) => 0 """ // error: no comp codomain in linear func
  )

  @test def typecheckApplication() = typecheck(
    """ (\(_: ()) (_: ()) (_: ()) => ()) () () () """ :|- "()",
    """ \(f: () -> ()) => f () """                    :|- "(() -> ()) -> ()",
    """ \(t: () -> ()) => \(c: ()) => t c """         :|- "(() -> ()) -> () -> ()"
  )

  @test def failApplication() = noType(
    """ (\(f: ()) => ()) 0 """ // error: expects () not Integer
  )

  @test def typecheckLinearLambda() = typecheck(
    """ \(a: A#) =>. a """                            :|- "forall a#. a# ->. a#",
    """ \(a: A#) =>. () """                           :|- "forall a#. a# ->. ()",
    """ \(_: A#) =>. () """                           :|- "forall a#. a# ->. ()",
    """ \(a: !A) =>. let !_ = a in \(a: ()) => a """  :|- "forall a. !a ->. () -> ()",
  )

  @test def failLinearLambda() = noType(
    """ \(a: A)  =>. () """, // error: a is not of computation type, so cant be in stoup
    """ \(c: C#) =>. !c """, // error: no dependency on c allowed
    """ \(a: A#) =>. \(b: B#) =>. b """, // error: rhs is not computational codomain
    """ \(a: ()) =>. \(b: ()) => \(c: ()) =>. a """, // error: no dependency on a allowed
    """ \(a: ()) =>. \(a: ()) => a """, // error: shadowing of linear a not allowed.
    """ \(_: A#) =>. 0 """, // error: `_` can not be in linear scope
    """ \(f: A -> B#) =>. \(x: !A) => let !y = x in f y """ // example from paper that wont type
  )

  @test def typecheckEval() = typecheck(
    """ (\(u: ()) =>. u)[()] """ :|- "()"
  )

  @test def typecheckLet() = typecheck(
    "let !x = !() in ()"  :|- "()",
    "let !x = !0 in !x"   :|- "!Integer",
  )

  @test def failLet() = noType(
    """ let !x = () in () """,  // error: () is not ! type
    """ let !x = !0 in 0 """,   // error: 0 is not of computation type
    """ \(u: ()) =>. let !_ = !() in u """, // error: no dependency on u allowed
  )

  @test def typecheckLetTensor() = typecheck(
    """ let !x *: y = !() *: () in
          !x *: y """               :|- "!() *: ()",
  )

  @test def failLetTensor() = noType(
    """ let !x *: y = () in
          !x *: y """, // error : () is not of tensor type
  )
}