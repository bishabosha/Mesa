package mesa.eec

import org.junit.Assert._
import org.junit.{ Test => test }

import Trees.Tree._

class InterpretorTest

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
    let !x be !${x = 1; 99} in x
    """
    Kernel.eval(program) match
    case Right(result) =>
      assert(x == 1)
      assert(result == 99)
    case Left(err) => throw err

  @test def tensorIsLazy: Unit =
    var xs = List.empty[Int]
    val program = eec"""
    !(let !t be !${xs ::= 0} in *) *: (let !t be !${xs ::= 1} in *)
    """
    Kernel.reduce(program) match
    case Right(term) =>
      assert(xs == Nil)
      assert(term == program)
    case Left(err) => throw err

  @test def letTensorEvaluatesOnlyLeft: Unit =
    var xs = List.empty[Int]
    val program = eec"""
    let !_ *: _ be !${xs ::= 0} *: (let !t be !${xs ::= 1} in *) in
    *
    """
    Kernel.reduce(program) match
    case Right(res) =>
      assert(xs == 0 :: Nil)
      assert(res == eec"*")
    case Left(err) => throw err

  @test def sequenceSideEffects: Unit =
    var xs = List.empty[Int]
    val program = eec"""
    let !x *: m be !${xs ::= 0; 35} *: (!${xs ::= 1}) in
    let !_      be m                                  in
    x
    """
    Kernel.eval(program) match
    case Right(res) =>
      assert(xs == 1 :: 0 :: Nil)
      assert(res == 35)
    case Left(err) => throw err

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
    (let !t be !${xs ::= 0} in *, let !t be !${xs ::= 1} in *)
    """
    Kernel.reduce(program) match
    case Right(term) =>
      assert(xs == Nil)
      assert(term == program)
    case Left(err) => throw err

  @test def fstOnlyEvaluatesLeft: Unit =
    var xs = List.empty[Int]
    val program = eec"""
    fst (Yes, let !x be !${xs ::= 0} in *)
    """
    Kernel.reduce(program) match
    case Right(term) =>
      assert(xs == Nil)
      assert(term == eec"Yes")
    case Left(err) => throw err

  @test def sndOnlyEvaluatesRight: Unit =
    var xs = List.empty[Int]
    val program = eec"""
    snd (let !x be !${xs ::= 0} in *, Yes)
    """
    Kernel.reduce(program) match
    case Right(term) =>
      assert(xs == Nil)
      assert(term == eec"Yes")
    case Left(err) => throw err

  @test def inlIsLazy: Unit =
    var xs = List.empty[Int]
    val program = eec"""
    inl (let !t be !${xs ::= 0} in *)
    """
    Kernel.reduce(program) match
    case Right(term) =>
      assert(xs == Nil)
      assert(term == program)
    case Left(err) => throw err

  @test def inrIsLazy: Unit =
    var xs = List.empty[Int]
    val program = eec"""
    inr (let !t be !${xs ::= 0} in *)
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
    let !result be !((^\x.x)[let !m be !${xs ::= 0; 56} in m]) in
    result
    """
    Kernel.eval(program) match
    case Right(result) =>
      assert(xs == 0 :: Nil)
      assert(result == 56)
    case Left(err) => throw err

  @test def caseExprReducesInl: Unit =
    var xs = List.empty[Int]
    val program = eec"""
    case inl (let !t be !${xs ::= 0} in Yes) of { inl l.l; inr _.* }
    """
    Kernel.reduce(program) match
    case Right(term) =>
      assert(xs == 0 :: Nil)
      assert(term == eec"Yes")
    case Left(err) => throw err

  @test def caseExprReducesInr: Unit =
    var xs = List.empty[Int]
    val program = eec"""
    case inr (let !t be !${xs ::= 0} in Yes) of { inl _.*; inr r.r }
    """
    Kernel.reduce(program) match
    case Right(term) =>
      assert(xs == 0 :: Nil)
      assert(term == eec"Yes")
    case Left(err) => throw err

  @test def inrIsNotBang: Unit =
    var xs = List.empty[Int]
    val program = eec"""
    case inr ${xs ::= 0} of { inl _.*; inr r.r }
    """
    Kernel.reduce(program) match
    case Right(term) => assert(PartialFunction.cond(term) { case Lazy(_) => xs == Nil })
    case Left(err)   => throw err

  @test def inlIsNotBang: Unit =
    var xs = List.empty[Int]
    val program = eec"""
    case inl ${xs ::= 0} of { inl l.l; inr _.* }
    """
    Kernel.reduce(program) match
    case Right(term) => assert(PartialFunction.cond(term) { case Lazy(_) => xs == Nil })
    case Left(err)   => throw err
