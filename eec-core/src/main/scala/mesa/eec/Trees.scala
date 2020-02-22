package mesa.eec

import mesa.util.StackMachine.{InterpretableK, Program, stack, Statement, Stack}
import Program.compile
import mesa.util.Show
import runtime.ScalaRunTime.stringOf

import annotation.tailrec

object Trees:
  import Tree._

  type Name = String

  private[eec] type ErasedTree = Tree[?]
  private[eec] def [T](e: ErasedTree).as: Tree[T] = e.asInstanceOf[Tree[T]]

  enum Tree[T] derives Eql
    case Point                                                                              extends Tree[Unit]
    case Pair[A, B](a: Tree[A], b: Tree[B])                                                 extends Tree[(A,B)]
    case Tensor[A,B](t: Tree[A], z: Tree[B])                                                extends Tree[(A,B)]
    case Var[T](name: Name)                                                                 extends Tree[T]
    case App[T, U](f: Tree[T => U], t: Tree[T])                                             extends Tree[U]
    case Eval[T, U](f: Tree[T => U], t: Tree[T])                                            extends Tree[U]
    case Lam[T,U](x: Name, t: Tree[U])                                                      extends Tree[T => U]
    case Lin[T,U](x: Name, t: Tree[U])                                                      extends Tree[T => U]
    case CaseExpr[L, R, U](e: Tree[Either[L, R]], x: Name, l: Tree[U], y: Name, r: Tree[U]) extends Tree[U]
    case Fst[A,B](p: Tree[(A,B)])                                                           extends Tree[A]
    case Snd[A,B](p: Tree[(A,B)])                                                           extends Tree[B]
    case Inl[A,B](a: Tree[A])                                                               extends Tree[Either[A,B]]
    case Inr[A,B](b: Tree[B])                                                               extends Tree[Either[A,B]]
    case Bang[T](t: Tree[T])                                                                extends Tree[T]
    case WhyNot[T](t: Tree[Nothing])                                                        extends Tree[T]
    case Let[T,U](n: Name, t: Tree[T], u: Tree[U])                                          extends Tree[U]
    case LetT[A,B,U](x: Name, z: Name, s: Tree[(A,B)], t: Tree[U])                          extends Tree[U]
    case Lazy[T](op: () => T)                                                               extends Tree[T]
    case Pure[T](x: T)                                                                      extends Tree[T]
    case Splice[T](index: Int)                                                              extends Tree[T]

  object Tree:

    given InterpretableK[Tree]:

      def [T,O](tree: Tree[T]) interpretK(z: O)(f: [t] => (O, Tree[t]) => O): O =
        @tailrec def inner(z: O, ts: List[ErasedTree]): O = ts match
          case Nil => z
          case t::ts => t match {
            case Pair(a,b)            => inner(f(z,t), a::b::ts)
            case Tensor(a,b)          => inner(f(z,t), a::b::ts)
            case Fst(p)               => inner(f(z,t), p::ts)
            case Snd(p)               => inner(f(z,t), p::ts)
            case Inl(l)               => inner(f(z,t), l::ts)
            case Inr(r)               => inner(f(z,t), r::ts)
            case Bang(a)              => inner(f(z,t), a::ts)
            case WhyNot(v)            => inner(f(z,t), v::ts)
            case App(g,u)             => inner(f(z,t), g::u::ts)
            case Eval(g,u)            => inner(f(z,t), g::u::ts)
            case Lam(_,u)             => inner(f(z,t), u::ts)
            case Lin(_,u)             => inner(f(z,t), u::ts)
            case CaseExpr(e,x,l,y,r)  => inner(f(z,t), e::l::r::ts)
            case Let(_,e,u)           => inner(f(z,t), e::u::ts)
            case LetT(_,_,e,u)        => inner(f(z,t), e::u::ts)
            case _                    => inner(f(z,t), ts)
          }
        end inner
        inner(z, tree::Nil)
      end interpretK
    end given

    given Show[ErasedTree] = summon[Show[Tree[Any]]].asInstanceOf[Show[ErasedTree]]

    given [T]: Show[Tree[T]]:

      def wrapIfComplex[U](t: Tree[U], s: String) = t match {
        case Point | _:Var[?] | _:Pure[?] | _:Lazy[?] | _:Pair[?,?] => s
        case _                                                      => s"($s)"
      }

      def wrapIfExpr1[U](t: Tree[U], s: String) = t match {
        case (
            _:Lam[?,?]
          | _:Lin[?,?]
          | _:Let[?,?]
          | _:LetT[?,?,?]
          | _:CaseExpr[?,?,?]
          | _:Fst[?,?]
          | _:Snd[?,?]
          | _:Inl[?,?]
          | _:Inr[?,?]) => s"($s)"
        case _ => s
      }

      def (t: Tree[T]) show: String = t.compile[Tree, T, String] ({

        case Pair(a,b)  =>
          val a1::b1::s1 = stack
          s"($a1, $b1)"::s1

        case Tensor(v,z) =>
          val v1::z1::s1 = stack
          val v2 = wrapIfComplex(v,v1)
          val z2 = wrapIfComplex(z,z1)
          s"!$v2 &: $z2"::s1

        case Fst(p) =>
          val p1::s1 = stack
          val p2 = wrapIfComplex(p,p1)
          s"fst $p2"::s1

        case Snd(p) =>
          val p1::s1 = stack
          val p2 = wrapIfComplex(p,p1)
          s"snd $p2"::s1

        case Bang(t) =>
          val t1::s1 = stack
          val t2 = wrapIfComplex(t,t1)
          s"!$t2"::s1

        case WhyNot(v) =>
          val v1::s1 = stack
          val v2 = wrapIfComplex(v,v1)
          s"?$v2"::s1

        case Inl(l) =>
          val l1::s1 = stack
          val l2 = wrapIfComplex(l,l1)
          s"inl $l2"::s1

        case Inr(r) =>
          val r1::s1 = stack
          val r2 = wrapIfComplex(r,r1)
          s"inr $r2"::s1

        case App(f,t) =>
          val f1::t1::s1 = stack
          val f2 = wrapIfExpr1(f,f1)
          val t2 = wrapIfComplex(t,t1)
          s"$f2 $t2"::s1

        case Eval(f,t) =>
          val f1::t1::s1 = stack
          val f2 = wrapIfExpr1(f, f1)
          s"$f2[$t1]"::s1

        case Lam(x,t) =>
          val t1::s1 = stack
          s"\\$x.$t1"::s1

        case Lin(x,t) =>
          val t1::s1 = stack
          s"^\\$x.$t1"::s1

        case CaseExpr(e,x,l,y,r) =>
          val e1::l1::r1::s1 = stack
          val e2 = wrapIfComplex(e, e1)
          s"case $e2 of {inl $x.$l1; inr $y.$r1}"::s1

        case Let(x,t,u) =>
          val t1::u1::s1 = stack
          val t2 = wrapIfComplex(t,t1)
          s"let !$x be $t2 in $u1"::s1

        case LetT(x,z,t,u) =>
          val t1::u1::s1 = stack
          val t2 = wrapIfComplex(t,t1)
          s"let !$x &: $z be $t2 in $u1"::s1

        case Point     => "()"                 :: stack
        case Pure(x)   => s"~(${stringOf(x)})" :: stack
        case Lazy(_)   => s"~<effect>"         :: stack
        case Splice(n) => s"~<splice:$n>"      :: stack
        case Var(x)    => x                    :: stack

      })
