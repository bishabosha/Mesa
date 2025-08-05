package mesa.eec

import quoted._
// import quoted.matching._

import java.io.StringReader

import Parsers._
import mesa.util.StackMachine.{InterpretableK, Program, stack, Statement, Stack}
import Program.compile
import Trees.Tree
import Tree._
import mesa.util.checkAtRuntime

object Macros {
  object ExprSeq {

    /** Matches a literal sequence of expressions and return a sequence of expressions.
     *
     *  Usage:
     *  ```scala
     *  inline def sum(args: Int*): Int = ${ sumExpr('args) }
     *  def sumExpr(argsExpr: Expr[Seq[Int]])(given QuoteContext): Expr[Int] = argsExpr match
     *    case ExprSeq(argExprs) =>
     *      // argExprs: Seq[Expr[Int]]
     *      ...
     *  }
     *  ```
     */
    def unapply[T: Type](expr: Expr[Seq[T]])(using qctx: Quotes): Option[Seq[Expr[T]]] = {
      import qctx.reflect.{_, given}
      def rec(tree: Term): Option[Seq[Expr[T]]] = tree match {
        case Repeated(elems, _) => Some(elems.map(x => x.asExprOf[T]))
        case Typed(e, _) => rec(e)
        case Block(Nil, e) => rec(e)
        case Inlined(_, Nil, e) => rec(e)
        case _  => None
      }
      rec(expr.asTerm.underlyingArgument)
    }

  }

  def eecImpl(scExpr: Expr[StringContext], argsExpr: Expr[Seq[Any]])(using qctx: Quotes): Expr[Tree[?]] = {
    import qctx.reflect.{Tree => _, *}

    def reify[T](t: Tree[T], args: Seq[Expr[Any]]): Expr[Tree[?]] = {
      import qctx.reflect.{Tree => _, _, given}
      val expr = t.compile[Expr[Tree[?]]] { [U] => (e: Tree[U]) => e match
        case Pair(_,_) =>
          (stack: @unchecked) match
          case '{ type t1; $a1: Tree[`t1`] } :: '{ type t2; $b1: Tree[`t2`] } :: s1 =>
            '{Pair[t1, t2]($a1, $b1)}::s1

        case Tensor(_,_) =>
          (stack: @unchecked) match
          case '{ $a1: Tree[t1] } :: '{ $b1: Tree[t2] } :: s1 =>
            '{Tensor[t1, t2]($a1, $b1)}::s1

        case App(_,_) =>
          (stack: @unchecked) match
          case '{ type t1; type t2; $f1: Tree[`t1` => `t2`] } :: s1 =>
            (s1: @unchecked) match
            case '{ type t3; $x1: Tree[`t3`] } :: s2 =>
              if TypeRepr.of[`t3`] <:< TypeRepr.of[`t1`] then
                '{App($f1, ${x1.asExprOf[Tree[t1]]})} :: s2
              else
                report.error(s"cannot apply argument of ${Type.show[t3]} to parameter of type ${Type.show[t1]}")
                Nil
          case '{ type t1; $f1: Tree[`t1`] }::_ =>
            report.error(s"cannot apply to non-function type ${Type.show[t1]}")
            Nil

        case Eval(_,_) =>
          (stack: @unchecked) match
          case '{ type t1; type t2; $f1: Tree[`t1` => `t2`] } :: s1 =>
            (s1: @unchecked) match
            case '{ type t3; $x1: Tree[`t3`] } :: s2 =>
              if TypeRepr.of[`t3`] <:< TypeRepr.of[`t1`] then
                '{Eval($f1, $x1.asInstanceOf[Tree[`t1`]])}::s2
              else
                report.error(s"cannot apply argument of ${Type.show[t3]} to parameter of type ${Type.show[t1]}")
                Nil
          case '{ type t1; $f1: Tree[`t1`] }::_ =>
            report.error(s"cannot linearly evaluate non-function type ${Type.show[t1]}")
            Nil

        case Lam(x1,_) =>
          (stack: @unchecked) match
          case '{ type t1; $a1: Tree[`t1`] } :: s1 =>
            '{Lam(${Expr(x1)}, $a1)}::s1

        case Lin(x1,_) =>
          (stack: @unchecked) match
          case '{ type t1; $a1: Tree[`t1`] } :: s1 =>
            '{Lin(${Expr(x1)}, $a1)}::s1

        case CaseExpr(_,x,_,y,_) =>
          (stack: @unchecked) match
          case '{ type t1; type t2; $e1: Tree[Either[`t1`, `t2`]] } :: s1 =>
            (s1: @unchecked) match
            case '{ type t3; $l1: Tree[`t3`] } :: s2 =>
              (s2: @unchecked) match
              case '{ $r1: Tree[`t3`] } :: s3 =>
                '{CaseExpr($e1, ${Expr(x)}, $l1, ${Expr(y)}, $r1)}::s1
              case _::_ =>
                report.error(s"body of case inr $y does not match case inl $x")
                Nil
          case '{ type t1; $e1: Tree[`t1`] } :: _ =>
            report.error(s"selector is not Either[?,?], but ${Type.show[t1]}")
            Nil

        case Let(x,_,_) =>
          (stack: @unchecked) match
          case '{ type t1; $a1: Tree[`t1`] } :: '{ type t2; $b1: Tree[`t2`] } :: s1 =>
            '{Let(${Expr(x)}, $a1, $b1)}::s1

        case LetT(x,z,_,_) =>
          (stack: @unchecked) match
          case '{ type t1; type t2; $a1: Tree[(`t1`, `t2`)] } :: '{ type t3; $b1: Tree[`t3`] } :: s1 =>
            '{LetT(${Expr(x)}, ${Expr(z)}, $a1, $b1)}::s1

        case Bang(t) =>
          (stack: @unchecked) match
          case '{ type t1; $a1: Tree[`t1`] } :: s1 =>
            '{Bang[t1]($a1)}::s1

        case WhyNot(t) =>
          (stack: @unchecked) match
          case '{ type t1 <: Nothing; $a1: Tree[`t1`] } :: s1 =>
            '{WhyNot($a1)}::s1
          case '{ type t1; $a1: Tree[`t1`] }::_ =>
            report.error(s"cannot apply ${Type.show[t1]} to argument of Nothing type")
            Nil

        case Point     => '{Point} :: stack
        case Fst()     => '{Fst()} :: stack
        case Snd()     => '{Snd()} :: stack
        case Inl()     => '{Inl()} :: stack
        case Inr()     => '{Inr()} :: stack
        case Splice(n) => '{lazy val x = ${args(n)}; Value(() => x)} :: stack
        case Value(x)  =>  { report.errorAndAbort(s"access to value from wrong staging level", Position.ofMacroExpansion) }
        case Var(x)    => '{Var[Any](${Expr(x)})} :: stack
      }
      expr.asExprOf[mesa.eec.Trees.Tree[?]]
    }

    def encode(parts: Seq[Expr[String]])(using Quotes): String = {
      val sb = new StringBuilder()

      def appendPart(part: Expr[String]) = {
        val value = part.valueOrAbort
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

    ((scExpr, argsExpr).checkAtRuntime) match {
      case ('{ StringContext($arg1*) }, args2) =>
        (arg1, args2) match {
          case (ExprSeq(parts), ExprSeq(args)) =>
            val code = encode(parts)
            parseAll(term[Any], StringReader(code)) match {
              case Success(matched,_) => reify(matched, args)
              case f:Failure          => sys.error(f.toString)
              case e:Error            => sys.error(e.toString)
            }
          case (e1, e2) =>
            report.errorAndAbort(s"expected a sequence of string literals, but got ${e1.show} and ${e2.show}", Position.ofMacroExpansion)
        }
    }
  }
}
