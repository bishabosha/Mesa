package mesa.eec

import mesa.util.StackMachine.{InterpretableK, Program, stack, Statement, Stack}
import Program.compile
import mesa.util.Show
import runtime.ScalaRunTime.stringOf

import annotation.tailrec

object Trees {
  import Tree._

  type Name       = String
  type ErasedTree = Tree[?]

  type Pi[A, B, I <: Int] <: A | B = I match {
    case 0 => A
    case 1 => B
    case _ => Nothing
  }

  enum Tree[T] derives Eql {

    case Point                                                                              extends Tree[Unit]
    case Pair[A, B](a: Tree[A], b: Tree[B])                                                 extends Tree[(A,B)]
    case Tensor[A,B](t: Tree[A], z: Tree[B])                                                extends Tree[(A,B)]
    case Var[T](name: Name)                                                                 extends Tree[T]
    case App[T, U](f: Tree[T => U], t: Tree[T])                                             extends Tree[U]
    case Eval[T, U](f: Tree[T => U], t: Tree[T])                                            extends Tree[U]
    case Lam[T,U](x: Name, t: Tree[U])                                                      extends Tree[T => U]
    case Lin[T,U](x: Name, t: Tree[U])                                                      extends Tree[T => U]
    case CaseExpr[L, R, U](e: Tree[Either[L, R]], x: Name, l: Tree[U], y: Name, r: Tree[U]) extends Tree[U]
    case Project[A,B,I <: Int & Singleton](p: Tree[(A,B)], elem: I)                         extends Tree[Pi[A,B,I]]
    case Inl[A,B]()                                                                         extends Tree[A => Either[A,B]]
    case Inr[A,B]()                                                                         extends Tree[B => Either[A,B]]
    case Bang[T]()                                                                          extends Tree[T => T]
    case WhyNot[T]()                                                                        extends Tree[Nothing => T]
    case Let[T,U](n: Name, t: Tree[T], u: Tree[U])                                          extends Tree[U]
    case LetT[A,B,U](x: Name, z: Name, s: Tree[(A,B)], t: Tree[U])                          extends Tree[U]
    case Lazy[T](x: () => T)                                                                extends Tree[T]
    case Pure[T](x: T)                                                                      extends Tree[T]
    case Splice[T](index: Int)                                                              extends Tree[T]

  }


  object Tree {

    given InterpretableK[Tree] {
      def [T,O](tree: Tree[T]) interpretK(z: O)(f: [t] => (O, Tree[t]) => O): O = {
        @tailrec
        def inner(z: O, ts: List[Tree[?]]): O = ts match {
          case Nil => z
          case t::ts => t match {
            case Pair(a,b)            => inner(f(z,t), a::b::ts)
            case Tensor(v,u)          => inner(f(z,t), v::u::ts)
            case Project(p, _)        => inner(f(z,t), p::ts)
            case App(g,u)             => inner(f(z,t), g::u::ts)
            case Eval(g,u)            => inner(f(z,t), g::u::ts)
            case Lam(_,u)             => inner(f(z,t), u::ts)
            case Lin(_,u)             => inner(f(z,t), u::ts)
            case CaseExpr(e,x,l,y,r)  => inner(f(z,t), e::l::r::ts)
            case Let(_,e,u)           => inner(f(z,t), e::u::ts)
            case LetT(_,_,e,u)        => inner(f(z,t), e::u::ts)
            case _                    => inner(f(z,t), ts)
          }
        }
        inner(z, tree::Nil)
      }
    }
  }

  given Show[ErasedTree] = summon[Show[Tree[Any]]].asInstanceOf[Show[ErasedTree]]

  given [T]: Show[Tree[T]] {
    import Tree._

    def wrapIfComplex[U](t: Tree[U], s: String) = t match {
      case Point | _:Var[?] | _:Pure[?] | _:Lazy[?] | _:Pair[?,?] => s
      case _                                                      => s"($s)"
    }

    def wrapIfExpr1[U](t: Tree[U], s: String) = t match {
      case _:Lam[?,?] | _:Lin[?,?] | _:Let[?,?] | _:LetT[?,?,?] | _:CaseExpr[?,?,?] => s"($s)"
      case _                                                                        => s
    }

    def (t: Tree[T]) show: String = t.compile[Tree, T, String] {

      case Pair(a,b)  =>
        val a1::b1::s1 = stack
        s"($a1, $b1)"::s1

      case Tensor(v,z) =>
        val v1::z1::s1 = stack
        val v2 = wrapIfComplex(v,v1)
        val z2 = wrapIfComplex(z,z1)
        s"!$v2 *: $z2"::s1

      case Project(p, e) =>
        val p1::s1 = stack
        s"${List("fst", "snd")(e)} $p1"::s1

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
        s"case $e1 of {inl $x.$l1; inr $y.$r1}"::s1

      case Let(x,t,u) =>
        val t1::u1::s1 = stack
        s"let !$x be $t1 in $u1"::s1

      case LetT(x,z,t,u) =>
        val t1::u1::s1 = stack
        s"let !$x *: $z be $t1 in $u1"::s1

      case Point     => "*"                   :: stack
      case Bang()    => "!"                   :: stack
      case WhyNot()  => "?"                   :: stack
      case Inl()     => "inl"                 :: stack
      case Inr()     => "inr"                 :: stack
      case Pure(x)   => s"~(${stringOf(x)})"  :: stack
      case Lazy(_)   => s"~<thunk>"           :: stack
      case Splice(n) => s"~<splice:$n>"       :: stack
      case Var(x)    => x                     :: stack

    }
  }
}
