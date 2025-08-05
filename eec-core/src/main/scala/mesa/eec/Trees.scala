package mesa.eec

import mesa.util.StackMachine.{InterpretableK, Program, stack, Statement, Stack}
import Program.compile
import mesa.util.Show
import mesa.util.checkAtRuntime

import annotation.tailrec

object Trees {

  type Name = String

  enum Tree[+T] derives CanEqual {
    case Point                                                                              extends Tree[Unit]
    case Pair[A, B](a: Tree[A], b: Tree[B])                                                 extends Tree[(A,B)]
    case Var[T](name: Name)                                                                 extends Tree[T]
    case App[T, U](f: Tree[T => U], t: Tree[T])                                             extends Tree[U]
    case Eval[T, U](f: Tree[T => U], t: Tree[T])                                            extends Tree[U]
    case Lam[T,U](x: Name, t: Tree[U])                                                      extends Tree[T => U]
    case Lin[T,U](x: Name, t: Tree[U])                                                      extends Tree[T => U]
    case CaseExpr[L, R, U](e: Tree[Either[L, R]], x: Name, l: Tree[U], y: Name, r: Tree[U]) extends Tree[U]
    case Fst[A,B]()                                                                         extends Tree[((A, B)) => A]
    case Snd[A,B]()                                                                         extends Tree[((A, B)) => B]
    case Inl[A,B]()                                                                         extends Tree[A => Either[A,B]]
    case Inr[A,B]()                                                                         extends Tree[B => Either[A,B]]
    case Bang[T](t: Tree[T])                                                                extends Tree[T]
    case WhyNot[T](e: Tree[Nothing])                                                        extends Tree[T]
    case Tensor[A,B](t: Tree[A], z: Tree[B])                                                extends Tree[(A,B)]
    case Let[T,U](n: Name, t: Tree[T], u: Tree[U])                                          extends Tree[U]
    case LetT[A,B,U](x: Name, z: Name, s: Tree[(A,B)], t: Tree[U])                          extends Tree[U]

    /* These trees are for purposes of embedding Scala values in a tree */
    case Value[T](x: () => T)                                                               extends Tree[T]
    case Splice[T](index: Int)                                                              extends Tree[T]
  }


  object Tree {

    given InterpretableK[Tree] {
      extension [T](tree: Tree[T]) def interpretK[O](z: O)(f: [t] => (O, Tree[t]) => O): O = {
        @tailrec
        def inner(z: O, ts: List[Tree[?]]): O = ts match {
          case Nil => z
          case t::ts => t match {
            case Pair(a,b)            => inner(f(z,t), a::b::ts)
            case App(g,u)             => inner(f(z,t), g::u::ts)
            case Eval(g,u)            => inner(f(z,t), g::u::ts)
            case Lam(_,u)             => inner(f(z,t), u::ts)
            case Lin(_,u)             => inner(f(z,t), u::ts)
            case CaseExpr(e,x,l,y,r)  => inner(f(z,t), e::l::r::ts)
            case Bang(u)              => inner(f(z,t), u::ts)
            case WhyNot(u)            => inner(f(z,t), u::ts)
            case Tensor(v,u)          => inner(f(z,t), v::u::ts)
            case Let(_,e,u)           => inner(f(z,t), e::u::ts)
            case LetT(_,_,e,u)        => inner(f(z,t), e::u::ts)
            case _                    => inner(f(z,t), ts)
          }
        }
        inner(z, tree::Nil)
      }
    }
  }

  given [T]: Show[Tree[T]] with {
    import Tree._

    def wrapIfComplex[U](t: Tree[U], s: String) = t match {
      case Point | _:Var[?] | _:Value[?] | _:Pair[?,?] => s
      case _                                         => s"($s)"
    }

    def wrapIfExpr1[U](t: Tree[U], s: String) = t match {
      case _:Lam[?,?] | _:Lin[?,?] | _:Let[?,?] | _:LetT[?,?,?] | _:CaseExpr[?,?,?] => s"($s)"
      case _                                                                        => s
    }

    extension (t: Tree[T]) def show: String = t.compile[String] { [U] => (t: Tree[U]) => t match
      case Pair(a,b)  =>
        val a1::b1::s1 = stack.checkAtRuntime
        s"($a1, $b1)"::s1

      case Tensor(v,z) =>
        val v1::z1::s1 = stack.checkAtRuntime
        val v2 = wrapIfComplex(v,v1)
        val z2 = wrapIfComplex(z,z1)
        s"!$v2 *: $z2"::s1

      case App(f,t) =>
        val f1::t1::s1 = stack.checkAtRuntime
        val f2 = wrapIfExpr1(f,f1)
        val t2 = wrapIfComplex(t,t1)
        s"$f2 $t2"::s1

      case Eval(f,t) =>
        val f1::t1::s1 = stack.checkAtRuntime
        val f2 = wrapIfExpr1(f, f1)
        s"$f2[$t1]"::s1

      case Lam(x,t) =>
        val t1::s1 = stack.checkAtRuntime
        s"\\$x.$t1"::s1

      case Lin(x,t) =>
        val t1::s1 = stack.checkAtRuntime
        s"^\\$x.$t1"::s1

      case CaseExpr(e,x,l,y,r) =>
        val e1::l1::r1::s1 = stack.checkAtRuntime
        s"case $e1 of {inl $x.$l1; inr $y.$r1}"::s1

      case Let(x,t,u) =>
        val t1::u1::s1 = stack.checkAtRuntime
        s"let !$x be $t1 in $u1"::s1

      case LetT(x,z,t,u) =>
        val t1::u1::s1 = stack.checkAtRuntime
        s"let !$x *: $z be $t1 in $u1"::s1

      case Bang(t) =>
        val t1::s1 = stack.checkAtRuntime
        val t2 = wrapIfComplex(t,t1)
        s"!$t2"::s1

      case WhyNot(t) =>
        val t1::s1 = stack.checkAtRuntime
        val t2 = wrapIfComplex(t,t1)
        s"?$t2"::s1

      case Point     => "*" :: stack
      case Fst()     => "fst" :: stack
      case Snd()     => "snd" :: stack
      case Inl()     => "inl" :: stack
      case Inr()     => "inr" :: stack
      case Value(x)  => s"{| ${x().toString} |}" :: stack
      case Splice(n) => s"<splice:$n>" :: stack
      case Var(x)    => x :: stack
    }
  }
}
