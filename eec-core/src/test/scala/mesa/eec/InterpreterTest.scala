package mesa.eec

import org.junit.Assert._
import org.junit.{ Test => test }

import Trees.Tree._

class InterpretorTest:

  @test def captureInPair: Unit =
    val program = eec"""
    (\f.\dup.
      f (dup Yes,())
    )
    (\p.fst (fst p))
    (\x.(x,x))
    """
    Kernel.reduce(program) match
    case Right(term) => assert(term == eec"""Yes""")
    case Left(err)   => throw err

  @test def stateMonad1: Unit =
    val program = eec"""
    (\put.\map.\run.
      run[map[put[!State]] (\_.Yes)]
    )
    (^\x.!() &: x)
    (^\t.\f.let !x &: n be t in !(f x) &: n)
    (^\t.let !_ &: m be t in m)
    """
    Kernel.reduce(program) match
    case Right(term) => assert(term == eec"""!State""")
    case Left(err)   => throw err

  @test def stateMonad2: Unit =
    val program = eec"""
    (\put.\map.\run.
      run[map[put[State]] (\_.Yes)]
    )
    (^\x.!() &: x)
    (^\t.\f.let !x &: n be t in !(f x) &: n)
    (^\t.let !_ &: z be t in ())
    """
    Kernel.reduce(program) match
    case Right(term) => assert(term == eec"""()""")
    case Left(err)   => throw err

  @test def spliceIsLazy: Unit =
    var x = 0
    val program = eec"${x == 1}"
    Kernel.reduce(program) match
    case Right(term) =>
      assert(x == 0)
      assert(term == program)
    case Left(err) => throw err

  @test def bangIsLazy: Unit =
    var x = 0
    val program = eec"!${x == 1}"
    Kernel.reduce(program) match
    case Right(term) =>
      assert(x == 0)
      assert(term == program)
    case Left(err) => throw err

  @test def whyNotIsLazy: Unit =
    var x = 0
    val program = eec"?${x == 1}"
    Kernel.reduce(program) match
    case Right(term) =>
      assert(x == 0)
      assert(term == program)
    case Left(err) => throw err

  @test def letBangEvaluates: Unit =
    var x = 0
    val program = eec"""
    let !_ be ${x = 1} in ()
    """
    Kernel.reduce(program) match
    case Right(result) =>
      assert(x == 1)
      assert(result == eec"()")
    case Left(err) => throw err

  @test def tensorIsLazy: Unit =
    var xs = List.empty[Int]
    val program = eec"""
    !(let !t be ${xs ::= 0} in ()) &: (let !t be ${xs ::= 1} in ())
    """
    Kernel.reduce(program) match
    case Right(term) =>
      assert(xs == Nil)
      assert(term == program)
    case Left(err) => throw err

  @test def letTensorEvaluatesOnlyLeft: Unit =
    var xs = List.empty[Int]
    val program = eec"""
    let !_ &: z be !No &: (let !t be ${xs ::= 0} in ()) in
    ()
    """
    Kernel.reduce(program) match
    case Right(res) =>
      assert(xs == Nil)
      assert(res == eec"()")
    case Left(err) => throw err

  @test def sequenceSideEffectsWithBang: Unit =
    var xs = List.empty[Int]
    val program = eec"""
    let !x be ${xs ::= 0} in
    let !_ be ${xs ::= 1} in
    ()
    """
    Kernel.reduce(program) match
    case Right(res) =>
      assert(xs == 1 :: 0 :: Nil)
      assert(res == eec"()")
    case Left(err) => throw err

  @test def onlyEvaluateLinearOnce: Unit =
    var xs = List.empty[Int]
    val program = eec"""
    (^\t.
    let !x be t in
    let !_ be t in
    ())[${xs ::= 0}]
    """
    Kernel.eval(program) match
    case Left(err) =>
      assert(xs == 0 :: Nil)
      assert(err.getMessage == "evaluating unbound value t in computational context")
    case Right(res) =>
      throw AssertionError(s"program terminated with $res")

  @test def noBangInLinear: Unit =
    var xs = List.empty[Int]
    val program = eec"""
    (^\t.let !_ be !() in ())[${xs ::= 0}]
    """
    Kernel.eval(program) match
    case Left(err) =>
      assertEquals(Nil, xs)
      assertEquals("unexpected !() in linear context", err.getMessage)
    case Right(res) =>
      throw AssertionError(s"program terminated with $res")

  @test def noLinearLambdaInLinear: Unit =
    var xs = List.empty[Int]
    val program = eec"""
    (^\t.let !_ be (^\z.t)[()] in ())[${xs ::= 0}]
    """
    Kernel.eval(program) match
    case Left(err) =>
      assertEquals(Nil, xs)
      assertEquals("unexpected ^\\z.t in linear context", err.getMessage)
    case Right(res) =>
      throw AssertionError(s"program terminated with $res")

  @test def haltUnreducibleVar: Unit =
    val program = eec"""
    No
    """
    Kernel.reduce(program) match
    case Right(term) => assert(term == program)
    case Left(err)   => throw err

  @test def appReduceLambda: Unit =
    val program = eec"""
    (\x.x) Yes
    """
    Kernel.reduce(program) match
    case Right(term) => assert(term == eec"Yes")
    case Left(err)   => throw err

  @test def evalReduceLinearLambda: Unit =
    val program = eec"""
    (^\x.x)[Yes]
    """
    Kernel.reduce(program) match
    case Right(term) => assert(term == eec"Yes")
    case Left(err)   => throw err

  @test def pairIsLazy: Unit =
    var xs = List.empty[Int]
    val program = eec"""
    (let !t be ${xs ::= 0} in (), let !t be ${xs ::= 1} in ())
    """
    Kernel.reduce(program) match
    case Right(term) =>
      assert(xs == Nil)
      assert(term == program)
    case Left(err) => throw err

  @test def fstOnlyEvaluatesLeft: Unit =
    var xs = List.empty[Int]
    val program = eec"""
    fst (Yes, let !x be ${xs ::= 0} in ())
    """
    Kernel.reduce(program) match
    case Right(term) =>
      assert(xs == Nil)
      assert(term == eec"Yes")
    case Left(err) => throw err

  @test def sndOnlyEvaluatesRight: Unit =
    var xs = List.empty[Int]
    val program = eec"""
    snd (let !x be ${xs ::= 0} in (), Yes)
    """
    Kernel.reduce(program) match
    case Right(term) =>
      assert(xs == Nil)
      assert(term == eec"Yes")
    case Left(err) => throw err

  @test def inlIsLazy: Unit =
    var xs = List.empty[Int]
    val program = eec"""
    inl (let !t be ${xs ::= 0} in ())
    """
    Kernel.reduce(program) match
    case Right(term) =>
      assert(xs == Nil)
      assert(term == program)
    case Left(err) => throw err

  @test def inrIsLazy: Unit =
    var xs = List.empty[Int]
    val program = eec"""
    inr (let !t be ${xs ::= 0} in ())
    """
    Kernel.reduce(program) match
    case Right(term) =>
      assert(xs == Nil)
      assert(term == program)
    case Left(err) => throw err

  /** If y leaked its scope then there would be an infinite loop */
  @test def variableDoesNotLeak: Unit =
    val program = eec"""
    (\y.y) ((\m.y) No)
    """
    Kernel.reduce(program) match
    case Right(term) => assert(term == eec"y")
    case Left(err)   => throw err

  @test def rememberClosure: Unit =
    val program = eec"""
    (\y.(\x.x) y) \z.z
    """
    Kernel.reduce(program) match
    case Right(term) => assert(term == eec"""\z.z""")
    case Left(err)   => throw err

  @test def nestedComputation: Unit =
    var xs = List.empty[Int]
    val program = eec"""
    let !_ be ((^\x.x)[let !m be ${xs ::= 0} in !m]) in
    ()
    """
    Kernel.reduce(program) match
    case Right(result) =>
      assert(xs == 0 :: Nil)
      assert(result == eec"()")
    case Left(err) => throw err

  @test def caseExprReducesInl: Unit =
    var xs = List.empty[Int]
    val program = eec"""
    case inl (let !t be ${xs ::= 0} in ()) of { inl l.l; inr _.() }
    """
    Kernel.reduce(program) match
    case Right(term) =>
      assert(xs == 0 :: Nil)
      assert(term == eec"()")
    case Left(err) => throw err

  @test def caseExprReducesInr: Unit =
    var xs = List.empty[Int]
    val program = eec"""
    case inr (let !t be ${xs ::= 0} in ()) of { inl _.(); inr r.r }
    """
    Kernel.reduce(program) match
    case Right(term) =>
      assert(xs == 0 :: Nil)
      assert(term == eec"()")
    case Left(err) => throw err

  @test def inrIsNotBang: Unit =
    var xs = List.empty[Int]
    val program = eec"""
    case inr ${xs ::= 0} of { inl _.(); inr r.r }
    """
    Kernel.reduce(program) match
    case Right(term) => assert(PartialFunction.cond(term) { case Lazy(_) => xs == Nil })
    case Left(err)   => throw err

  @test def inlIsNotBang: Unit =
    var xs = List.empty[Int]
    val program = eec"""
    case inl ${xs ::= 0} of { inl l.l; inr _.() }
    """
    Kernel.reduce(program) match
    case Right(term) => assert(PartialFunction.cond(term) { case Lazy(_) => xs == Nil })
    case Left(err)   => throw err
