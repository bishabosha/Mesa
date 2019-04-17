package eec
package compiler
package types

import ExprBootstraps._
import BootstrapTests._

import org.junit.Test
import org.junit.Assert._

class ExprTest {

  @Test def typecheckInteger() = typecheck(
    "0"  :|- "Integer",
    "-0" :|- "Integer"
  )

  @Test def failInteger() = noParse(
    "0l", // error: no Longs
    "0L"  // error: no Longs
  )

  @Test def typecheckDecimal() = typecheck(
    "3.14159265358979323846264338328" :|- "Decimal", // PI
    "6.62607004e-34"                  :|- "Decimal", // Planck's constant
    "-273.15"                         :|- "Decimal"  // 0 degrees Kelvin
  )

  @Test def failDecimal() = noParse(
    "3.14159f", // error: no Floats
    "3.14159F", // error: no Floats
    "3.14159d", // error: no Doubles
    "3.14159D"  // error: no Doubles
  )

  @Test def typecheckBoolean() = typecheck(
    "True"  :|- "Boolean",
    "False" :|- "Boolean"
  )

  @Test def typecheckChar() = typecheck(
    """ 'a' """  :|- "Char",
    """ '\n' """ :|- "Char"
  )

  @Test def failChar() = noParse(
    "''",   // error: empty char
    "'ab'"  // error: char more than one char
  )

  @Test def typecheckString() = typecheck(
    """ "test" """       :|- "String",
    "\"\"\"test\"\"\""   :|- "String"
  )

  @Test def typecheckProducts() = typecheck(
    "()"          :|- "()",
    "(())"        :|- "()",
    "((),())"     :|- "((), ())",
    "((),(),())"  :|- "((), (), ())",
  )

  @Test def typecheckTensor() = typecheck(
    "!() *: ()" :|- "!() *: ()",
  )

  @Test def failTensor() = noType(
    "!0 *: 0", // error: 0 is not of computation type
  )

  @Test def failTensorParse() = noParse(
    "0 *: ()", // error: syntax error, expecting !
  )

  @Test def typecheckCompute() = typecheck(
    "!()"       :|- "!()",
    "!((),())"  :|- "!((), ())",
    "(!(),!())" :|- "(!(), !())",
  )

  @Test def failCompute() = noType(
    """ \(_: ! a b) => () """, // error: expected types [_] but got [a, b]
    """ ! () () """, // error: expected args [_: _] but got [(): (), (): ()]
    """ \(x: A#) -○ !x """, // error: `!t` can't depend on linear variable
  )

  @Test def typecheckIf() = typecheck(
    "if True then () else ()" :|- "()"
  )

  @Test def failIf() = noType(
    "if 0 then () else ()",   // error: non Boolean condition
    "if True then 0 else ()"  // error: disjoint branches
  )

  @Test def typecheckCase() = typecheck(
    "()"       -|:  """ case () of
                          _ => () """,

    "()"       -|:  """ case () of
                          x => x """,

    "((), ())" -|:  """ case () of
                          c @ x => (c, x) """,

    "()"       -|:  """ case () of
                          _ if True => () """,

    "()"       -|:  """ case () of
                          ( ) | () => () """,

    "()"       -|:  """ case ((), ()) of
                          (_, a) => a """,

    "()"       -|:  """ case ((), ((), ())) of
                          (_, (_, a)) => a """,

    "((), ())" -|:  """ case ((), ((), ())) of
                          (a, (_, b)) => (a, b) """,

    "()"       -|:  """ case ((), (), ()) of
                          (_, _, _) => () """,

    "()"       -|:  """ case Left () of
                          Left  x => x;
                          Right _ => () """,

    "()"       -|:  """ case Right () of
                          Right x => x;
                          Left  _ => () """,

    "()"       -|:  """ case False of
                          True => ();
                          _    => () """,

    "()"       -|:  """ case True of
                          False => ();
                          _     => () """,
  )

  @Test def failCase() = noType(
    """ case Left () of
         Left  x => x """, // error: missing [Right _]

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

  @Test def typecheckCoTensor() = typecheck(
    """ InR [InL [()]] """ :|- "<L#:2> +: () +: <R#:4>",
    """ InL [InR [()]] """ :|- "(<L#:8> +: ()) +: <R#:6>",
  )

  @Test def typecheckLinearCase() = typecheck(
    """ case ((), ()) of
          (x, _) -○ x """            :|- "()",

    """ case ((),()) of
          (x, ( )) -○ x """          :|- "()",

    """ case InR [((), ())] of
          InR[(x, _)] -○ x
          InL[u]      -○ () """      :|- "()",

    """ case (InR [()], ()) of
          (InR[x], _) -○ x
          (InL[u], _) -○ () """      :|- "()",

    """ case InL [()] of
          InL[n] -○ n
          InR[u] -○ () """          :|- "()",
  )

  @Test def failLinearCase() = noType(
    """ case InL [()] of
          InL[n] -○ n""", // error: missing [InR _]

    """ case InR [()] of
          InR[n] -○ n""", // error: missing [InL _]

    """ case (0, ()) of
          (x, _) -○ x """,    // error: `x: Integer` not allowed in stoup

    """ case ((), ()) of
          (x, y) -○ x """,    // error: can't put x and y together in stoup
  )

  @Test def typecheckLambda() = typecheck(
    """ \(t: ()) => () """                :|- "() -> ()",
    """ \(_: ()) => () """                :|- "() -> ()",
    """ \(t: ()) => t """                 :|- "() -> ()",
    """ \(_: () -> ()) => () """          :|- "(() -> ()) -> ()",
    """ \(_: () -○ ()) => () """          :|- "(() -○ ()) -> ()",
    """ \(_: ()) (_: ()) => () """        :|- "() -> () -> ()",
    """ \(a: ()) (b: ()) => (a,b) """     :|- "() -> () -> ((), ())",
  )

  @Test def typecheckApplication() = typecheck(
    """ (\(_: ()) (_: ()) (_: ()) => ()) () () () """ :|- "()",
    """ \(f: () -> ()) => f () """                    :|- "(() -> ()) -> ()",
    """ \(t: () -> ()) => \(c: ()) => t c """         :|- "(() -> ()) -> () -> ()"
  )

  @Test def failApplication() = noType(
    """ (\(f: ()) => ()) 0 """ // error: expects () not Integer
  )

  @Test def typecheckLinearLambda() = typecheck(
    """ \(a: A#) -○ a """  :|- "A# -○ A#",
    """ \(b: B#) -○ () """ :|- "B# -○ ()",
  )

  @Test def failLinearLambda() = noType(
    """ \(a: A)  -○ () """, // error: a is not of computation type, so cant be in stoup
    """ \(c: C#) -○ !c """, // error: no dependency on c allowed
    """ \(a: A#) -○ \(b: B#) -○ b """, // error: rhs is not computational codomain
    """ \(a: ()) -○ \(b: ()) => \(c: ()) -○ a """, // error: no dependency on a allowed
    """ \(_: A#) -○ () """, // error: Illegal wildcard var name in stoup
  )

  @Test def typecheckEval() = typecheck(
    """ (\(u: ()) -○ u)[()] """ :|- "()"
  )

  @Test def typecheckLet() = typecheck(
    "let !x = !() in ()"  :|- "()",
    "let !x = !0 in !x"   :|- "!Integer",
  )

  @Test def failLet() = noType(
    """ let !x = () in () """,  // error: () is not ! type
    """ let !x = !0 in 0 """,   // error: 0 is not of computation type
    """ \(u: ()) -○ let !_ = !() in u """, // error: no dependency on u allowed
  )

  @Test def typecheckLetTensor() = typecheck(
    """ let !x *: y = !() *: () in
          !x *: y """               :|- "!() *: ()",
  )

  @Test def failLetTensor() = noType(
    """ let !x *: y = () in
          !x *: y """, // error : () is not of tensor type
  )
}