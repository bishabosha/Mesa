package mesa.eec

import quoted._
import quoted.matching._

import java.io.StringReader

import Parsers._
import mesa.util.StackMachine.{InterpretableK, Program, stack, Statement, Stack}
import Program.compile
import Trees.Tree
import Tree._

object Macros {
  def eecImpl(scExpr: Expr[StringContext], argsExpr: Expr[Seq[Any]]) given (qctx: QuoteContext): Expr[Tree[?]] = {

    def reify[U](t: Tree[U], args: Seq[Expr[Any]]): Expr[Tree[?]] = {
      import qctx.tasty.{Tree => _, _}
      val expr = t.compile[Tree, U, Expr[Tree[?]]] {
        case Pair(_,_)  =>
          val '{ $a1: Tree[$t1] } :: '{ $b1: Tree[$t2] } :: s1 = stack
          '{Pair[$t1, $t2]($a1.asInstanceOf, $b1.asInstanceOf)}::s1

        case Tensor(_,_) =>
          val '{ $a1: Tree[$t1] } :: '{ $b1: Tree[$t2] } :: s1 = stack
          '{Tensor[$t1, $t2]($a1.asInstanceOf, $b1.asInstanceOf)}::s1

        case App(_,_) =>
          val '{ $f1: Tree[$t1] } :: '{ $x1: Tree[$t2] } :: s1 = stack
          '{App($f1.asInstanceOf, $x1.asInstanceOf)}::s1

        case Eval(_,_) =>
          val '{ $f1: Tree[$t1] } :: '{ $x1: Tree[$t2] } :: s1 = stack
          '{Eval($f1.asInstanceOf, $x1.asInstanceOf)}::s1

        case Lam(x1,_) =>
          val '{ $a1: Tree[$t1] } ::s1 = stack
          '{Lam(${x1.toExpr}, $a1.asInstanceOf)}::s1

        case Lin(x1,_) =>
          val '{ $a1: Tree[$t1] } ::s1 = stack
          '{Lin(${x1.toExpr}, $a1.asInstanceOf)}::s1

        case CaseExpr(_,x,_,y,_) =>
          val '{ $e1: Tree[$t1] } :: '{ $l1: Tree[$t2] } :: '{ $r1: Tree[$t3] } :: s1 = stack
          '{CaseExpr($e1.asInstanceOf,${x.toExpr}, $l1.asInstanceOf, ${y.toExpr}, $r1.asInstanceOf)}::s1

        case Let(x,_,_) =>
          val '{ $a1: Tree[$t1] } :: '{ $b1: Tree[$t2] } :: s1 = stack
          '{Let(${x.toExpr},$a1.asInstanceOf, $b1.asInstanceOf)}::s1

        case LetT(x,z,_,_) =>
          val '{ $a1: Tree[$t1] } :: '{ $b1: Tree[$t2] } :: s1 = stack
          '{LetT(${x.toExpr}, ${z.toExpr}, $a1.asInstanceOf, $b1.asInstanceOf)}::s1

        case Bang(t) =>
          val '{ $a1: Tree[$t1] } :: s1 = stack
          '{Bang[$t1]($a1.asInstanceOf)}::s1

        case WhyNot(t) =>
          val '{ $a1: Tree[$t1] } :: s1 = stack
          '{WhyNot($a1.asInstanceOf)}::s1

        case Point     => '{Point} :: stack
        case Fst()     => '{Fst()} :: stack
        case Snd()     => '{Snd()} :: stack
        case Inl()     => '{Inl()} :: stack
        case Inr()     => '{Inr()} :: stack
        case Splice(n) => '{lazy val x = ${args(n)}; Value(() => x)} :: stack
        case Value(x)  =>  { error(s"access to value from wrong staging level", rootPosition); ??? }
        case Var(x)    => '{Var(${x.toExpr})} :: stack
      }
      expr.cast[mesa.eec.Trees.Tree[?]]
    }

    def encode(parts: Seq[Expr[String]]) given QuoteContext: String = {
      val sb = new StringBuilder()

      def appendPart(part: Expr[String]) = {
        val Const(value: String) = part
        sb ++= value
      }

      def appendHole(index: Int) = {
        sb ++= Hole.encode(index)
      }

      for ((part, index) <- parts.init.zipWithIndex) {
        appendPart(part)
        appendHole(index)
      }
      appendPart(parts.last)

      sb.toString
    }

    ((scExpr, argsExpr): @unchecked) match {
      case ('{ StringContext(${ExprSeq(parts)}: _*) }, ExprSeq(args)) =>
        val code = encode(parts)
        parseAll(term[Any], StringReader(code)) match {
          case Success(matched,_) => reify(matched, args)
          case f:Failure          => sys.error(f.toString)
          case e:Error            => sys.error(e.toString)
        }
    }
  }
}