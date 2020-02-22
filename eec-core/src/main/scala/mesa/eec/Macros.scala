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
  def eecImpl(scExpr: Expr[StringContext], argsExpr: Expr[Seq[Any]])(given qctx: QuoteContext): Expr[Tree[Any]] = {

    def reifyErased[U: Type](t: Tree[U], args: Seq[Expr[Any]]): Expr[Tree[U]] = {
      import qctx.tasty.{Tree => _, Singleton => _, error => _, _, given}
      val expr = t.compile[Tree, U, Expr[Tree[?]]] {
        case Pair(_,_) =>
          (stack: @unchecked) match
          case '{ $a1: Tree[$t1] } :: '{ $b1: Tree[$t2] } :: s1 =>
            '{Pair[$t1, $t2]($a1, $b1)}::s1

        case Tensor(_,_) =>
          (stack: @unchecked) match
          case '{ $a1: Tree[$t1] } :: '{ $b1: Tree[$t2] } :: s1 =>
            '{Tensor[$t1, $t2]($a1, $b1)}::s1

        case Fst(_) =>
          (stack: @unchecked) match
          case '{ $p1: Tree[$t1] } :: s1 =>
            '{Fst($p1.asInstanceOf[Tree[(Any, Any)]])} :: s1

        case Snd(_) =>
          (stack: @unchecked) match
          case '{ $p1: Tree[$t1] } :: s1 =>
            '{Snd($p1.asInstanceOf[Tree[(Any, Any)]])} :: s1

        case Bang(_) =>
          (stack: @unchecked) match
          case '{ $a1: Tree[$t1] } :: s1 =>
            '{Bang($a1)} :: s1

        case WhyNot(_) =>
          (stack: @unchecked) match
          case '{ $v1: Tree[$t1] } :: s1 =>
            '{WhyNot($v1.asInstanceOf[Tree[Nothing]])} :: s1

        case Inl(_) =>
          (stack: @unchecked) match
          case '{ $a1: Tree[$t1] } :: s1 =>
            '{Inl[$t1, Any]($a1)} :: s1

        case Inr(_) =>
          (stack: @unchecked) match
          case '{ $a1: Tree[$t1] } :: s1 =>
            '{Inr[Any, $t1]($a1)} :: s1

        case App(_,_) =>
          (stack: @unchecked) match
          case '{ $f1: Tree[$t1] } :: '{ $x1: Tree[$t2] } :: s1 =>
            '{App($f1.asInstanceOf[Tree[$t2 => Any]], $x1)} :: s1

        case Eval(_,_) =>
          (stack: @unchecked) match
          case '{ $f1: Tree[$t1] } :: '{ $x1: Tree[$t2] } :: s1 =>
            '{Eval($f1.asInstanceOf[Tree[$t2 => Any]], $x1)} :: s1

        case Lam(x1,_) =>
          (stack: @unchecked) match
          case '{ type $t1; $a1: Tree[`$t1`] } :: s1 =>
            '{Lam(${Expr(x1)}, $a1)} :: s1

        case Lin(x1,_) =>
          (stack: @unchecked) match
          case '{ type $t1; $a1: Tree[`$t1`] } :: s1 =>
            '{Lin(${Expr(x1)}, $a1)} :: s1

        case CaseExpr(_,x,_,y,_) =>
          (stack: @unchecked) match
          case '{ $e1: Tree[$t1] } :: '{ $l1: Tree[$t2] } :: '{ $r1: Tree[$t3] } :: s1 =>
            '{CaseExpr($e1.asInstanceOf[Tree[Either[Any,Any]]], ${Expr(x)}, $l1.asInstanceOf[Tree[Any]], ${Expr(y)}, $r1.asInstanceOf[Tree[Any]])} :: s1

        case Let(x,_,_) =>
          (stack: @unchecked) match
          case '{ $a1: Tree[$t1] } :: '{ $b1: Tree[$t2] } :: s1 =>
            '{Let(${Expr(x)}, $a1, $b1)} :: s1

        case LetT(x,z,_,_) =>
          (stack: @unchecked) match
          case '{ $a1: Tree[$t1] } :: '{ $b1: Tree[$t2] } :: s1 =>
            '{LetT(${Expr(x)}, ${Expr(z)}, $a1.asInstanceOf[Tree[(Any, Any)]], $b1)} :: s1

        case Point     => '{Point}              :: stack
        case Splice(n) => '{effect(${args(n)})} :: stack
        case Var(x)    => '{Var(${Expr(x)})}    :: stack
        case Pure(x)   =>  { qctx.error(s"Access to value $x from wrong staging level"); ??? }
        case Lazy(_)   =>  { qctx.error(s"Access to thunk from wrong staging level"); ??? }
      }
      expr.asInstanceOf[Expr[mesa.eec.Trees.Tree[U]]]
    }

    def encode(parts: Seq[Expr[String]])(given QuoteContext): String =
      val sb = StringBuilder()
      def appendPart(part: Expr[String]): Unit =
        val Const(value: String) = part
        sb ++= value
      def appendHole(index: Int): Unit =
        sb ++= Hole.encode(index)
      for (part, index) <- parts.init.zipWithIndex do
        appendPart(part)
        appendHole(index)
      appendPart(parts.last)
      sb.toString
    end encode

    ((scExpr, argsExpr): @unchecked) match {
      case ('{ StringContext(${ExprSeq(parts)}: _*) }, ExprSeq(args)) =>
        val code = encode(parts)
        val reader = StringReader(code)
        try
          parseAll(term[Any], reader) match {
            case Success(matched,_) => reifyErased(matched, args)
            case f:Failure          => sys.error(f.toString)
            case e:Error            => sys.error(e.toString)
          }
        finally
          reader.close()
    }
  }
}
