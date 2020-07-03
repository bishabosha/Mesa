package mesa.eec

import org.junit.Assert._
import org.junit.{ Test => test }

import Trees.Tree._

class InterpolatorTest:

  @test def spliceIsLazy: Unit =
    eec"${1}" match
    case Lazy(t) => assert(t() == 1)
    case tree    => fail(s"Splice was $tree")

  @test def xIsVar: Unit =
    assert(eec"x" == Var("x"))

  @test def lambda1: Unit =
    assert(eec"""\x.x""" == Lam("x", Var("x")))

  @test def lambda2: Unit =
    assert(eec"""^\x.x""" == Lin("x", Var("x")))

  @test def app: Unit =
    assert(eec"""(\x.x) Yes""" == App(Lam("x", Var("x")), Var("Yes")))

  @test def eval: Unit =
    assert(eec"""(^\x.x)[Yes]""" == Eval(Lin("x", Var("x")), Var("Yes")))

  @test def pair: Unit =
    assert(eec"""(x,x)""" == Pair(Var("x"),Var("x")))

  @test def tensor: Unit =
    assert(eec"""!x &: y""" == Tensor(Var("x"),Var("y")))

  @test def point: Unit =
    assert(eec"""()""" == Point)

  @test def bang: Unit =
    assert(eec"""!()""" == Bang(Point))

  @test def bangSplice: Unit =
    var x = 0
    eec"""!${x = 1}""" match
    case Bang(Lazy(_)) =>
      assert(x == 0)
    case tree =>
      fail(s"bang with splice was $tree")

  @test def whyNot: Unit =
    assert(eec"""?x""" == WhyNot(Var("x")))

  @test def inl: Unit =
    assert(eec"""inl ()""" == Inl(Point))

  @test def inr: Unit =
    assert(eec"""inr ()""" == Inr(Point))

  @test def caseexpr: Unit =
    assert(eec"""case x of {inl l.l; inr r.r}""" == CaseExpr(Var("x"), "l", Var("l"), "r", Var("r")))

  @test def let: Unit =
    assert(eec"""let !x be m in x""" == Let("x", Var("m"), Var("x")))

  @test def letT: Unit =
    assert(eec"""let !x &: y be m in y""" == LetT("x", "y", Var("m"), Var("y")))

  @test def fst: Unit =
    assert(eec"""fst x""" == Fst(Var("x")))

  @test def snd: Unit =
    assert(eec"""snd x""" == Snd(Var("x")))
