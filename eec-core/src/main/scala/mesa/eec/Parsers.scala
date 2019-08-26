package mesa.eec

import language.implicitConversions

import util.parsing.combinator.JavaTokenParsers

import Trees.Tree
import Tree._

object Parsers extends JavaTokenParsers {
  type P[T] = Parser[Tree[T]]

  def term[T]: P[T]  = phrase(expr)
  def expr[T]: P[T]  = app
  def expr1[T]: P[T] = (lam | lin | let | letT | cse | expr2).asInstanceOf[P[T]]
  def expr2[T]: P[T] = (tsor | pExpr).asInstanceOf[P[T]]
  def pExpr[T]: P[T] = (bang | whyNot | fst | snd | inl | inr | aexpr).asInstanceOf[P[T]]
  def aexpr[T]: P[T] = (ref | unit | pair | wrap).asInstanceOf[P[T]]

  def app[T]: P[T] = rep1(expr1)                     ^^ { case ts => ts.reduceLeft(([t, u] => (f: Tree[t => u], a: Tree[t]) => App(f,a)).asInstanceOf).asInstanceOf }
  def lam[T,U]: P[T => U] = "\\"~>!id~!"."~!expr[U]  ^^ { case x~_~t => Lam(x,t) }
  def lin[T,U]: P[T => U] = "^\\"~>!id~!"."~!expr[U] ^^ { case x~_~t => Lin(x,t) }

  def let[T,U]: P[U] = "let"~>"!"~>id~"be"~!expr~!"in"~!expr[U] ^^ {
    case x~_~t~_~u => Let(x,t,u)
  }

  def letT[T,U,V]: P[V] = "let"~>"!"~>id~"*:"~!id~!"be"~!expr[(T,U)]~!"in"~!expr[V] ^^ {
    case x~_~y~_~s~_~t => LetT(x,y,s,t)
  }

  def cse[L,R,U]: P[U] = {
    "case"~>!expr[Either[L,R]]~!"of"~!"{"~!
      "inl"~!id~!"."~!expr[U]~!";"~!
      "inr"~!id~!"."~!expr[U]<~!
    "}" ^^ { case e~_~_~_~x~_~l~_~_~y~_~r => CaseExpr(e,x,l,y,r) }
  }

  def bang[T]: P[T] = "!" ~> aexpr[T] ^^ { case e => Bang(e) }
  def whyNot[T]: P[Nothing => T] = "?" ~> aexpr[Nothing] ^^ { case e => WhyNot(e) }
  def fst[A,B]: P[A] = "fst" ~> aexpr[(A,B)] ^^ { case e => App(Fst(),e) }
  def snd[A,B]: P[B] = "snd" ~> aexpr[(A,B)] ^^ { case e => App(Snd(),e) }
  def inl[A,B]: P[Either[A,B]] = "inl" ~> aexpr[A] ^^ { case e => App(Inl(),e) }
  def inr[A,B]: P[Either[A,B]] = "inr" ~> aexpr[B] ^^ { case e => App(Inr(),e) }

  def tsor[A,B]: P[(A,B)] = "!"~>aexpr[A]~"*:"~!aexpr[B]    ^^ { case t~_~z => Tensor(t,z) }
  def pair[A,B]: P[(A,B)] = "("~>expr[A]~","~!expr[B]<~!")" ^^ { case t~_~u => Pair(t,u)   }
  def wrap[T]: P[T] = "("~>expr[T]<~")"                     ^^ { x          => x           }
  def unit: P[Unit] = "*"                                   ^^ { _          => Point       }
  def ref[T]:  P[T] = id                                    ^^ {               Var(_)      }

  val reservedWords = Set(
    "case", "of", "let", "be", "in", "fst", "snd", "inl", "inr"
  )

  val id = Parser { input =>
    ident(input).filterWithError(
      !reservedWords.contains(_),
      reservedWord => s"inappropriate use of $reservedWord",
      input
    )
  }
}

object EEC {
  import Parsers._
  import java.io.StringReader

  def (s: String) eec: Tree[Any] | String = parseAll(term[Any], StringReader(s)) match {
    case Success(matched,_) => matched
    case f:Failure          => f.toString
    case e:Error            => e.toString
  }

  def (r: Tree[Any] | String) ! : Tree[Any] = r match {
    case matched: Tree[_] => matched
    case err: String   => sys.error(err)
  }
}