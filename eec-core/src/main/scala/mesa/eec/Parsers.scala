package mesa.eec

import language.implicitConversions

import util.parsing.combinator.JavaTokenParsers

import Trees.Tree
import Tree._

object Hole {
  val HoleStart = 0xE000.toChar.toString
  val HoleChar  = 0xE001.toChar.toString
  def encode(i: Int) = HoleStart + HoleChar * i
}

object Parsers extends JavaTokenParsers {
  import Hole._

  type P[T] = Parser[Tree[T]]

  def term[T]: P[T]  = phrase(expr)
  def expr[T]: P[T]  = app
  def expr1[T]: P[T] = (lam | lin | let | letT | cse | expr2).asInstanceOf[P[T]]
  def expr2[T]: P[T] = (tsor | pExpr).asInstanceOf[P[T]]
  def pExpr[T]: P[T] = (bang | whyNot | fst | snd | inl | inr | eval | aexpr).asInstanceOf[P[T]]
  def aexpr[T]: P[T] = (ref | unit | pair | wrap | splice).asInstanceOf[P[T]]

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

  def bang[T]: P[T]            = "!"   ~> aexpr[T]       ^^ { case e => Bang(e)      }
  def whyNot[T]: P[T]          = "?"   ~> aexpr[Nothing] ^^ { case e => WhyNot(e)    }
  def fst[A,B]: P[A]           = "fst" ~> aexpr[(A,B)]   ^^ { case e => App(Fst(),e) }
  def snd[A,B]: P[B]           = "snd" ~> aexpr[(A,B)]   ^^ { case e => App(Snd(),e) }
  def inl[A,B]: P[Either[A,B]] = "inl" ~> aexpr[A]       ^^ { case e => App(Inl(),e) }
  def inr[A,B]: P[Either[A,B]] = "inr" ~> aexpr[B]       ^^ { case e => App(Inr(),e) }

  def tsor[A,B]: P[(A,B)] = "!"~>aexpr[A]~"*:"~!aexpr[B]          ^^ { case t~_~z => Tensor(t,z)     }
  def pair[A,B]: P[(A,B)] = "("~>expr[A]~","~!expr[B]<~!")"       ^^ { case t~_~u => Pair(t,u)       }
  def eval[T,U]: P[U]     = aexpr[T => U] ~ "[" ~! expr[T] <~ "]" ^^ { case f~_~t => Eval(f,t)       }
  def wrap[T]: P[T]       = "("~>expr[T]<~")"                     ^^ { x          => x               }
  def unit: P[Unit]       = "*"                                   ^^ { _          => Point           }
  def ref[T]:  P[T]       = id                                    ^^ {               Var(_)          }
  def splice[T]           = HoleStart ~> HoleChar.*               ^^ { case cs  => Splice(cs.length) }

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
