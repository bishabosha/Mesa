package mesa.eec

import Trees._, Tree._
import deriving.Mirror
import mesa.util.Show

final class TypeError(msg: String) extends RuntimeException(msg)

object Kernel

  case class Closure(x: Name, t: ErasedTree, e: Environment)

  type Environment = List[Closure]
  type State       = (ErasedTree, Environment, List[(ErasedTree, Environment)])

  given (given m: Mirror.ProductOf[Closure], rec: => Show[m.MirroredElemTypes]): Show[Closure] =
    Tuple.fromProductTyped(_).show

  given Show[State] =
    Show.t3

  private def start[T](t: Tree[T]): State = (t, Nil, Nil)

  private def typeError(msg: String): Nothing = throw TypeError(msg)

  private def (s: State) step: State = (s: @unchecked) match

  case ( v @ Var(x),       Closure(y,n,f)::e, s        ) => if x == y then (n,f,s) else (v,e,s)
  case ( Lambda(x,t),      e,                 (m,f)::s ) => (t, Closure(x,m,f)::e, s       )
  case ( Application(n,m), e,                 s        ) => (n, e,                 (m,e)::s)

  case ( Project(p,i), e, s ) => rec(p, e, s) match
    case p: Pair[a,b] => (if i == 0 then p._1 else p._2, e, s)
    case v: Var[a]    => (Project(v.as, i), e, s)
    case arg          => typeError(s"expected (_,_) as argument to ${List("fst","snd")(i)} but got: ${arg.show}")

  case ( CaseExpr(p,x,l,y,r), e, s ) => rec(p, e, s) match
    case Application(Inl(), inl) => (l, bind(e, x -> rec(inl, e, s)), s)
    case Application(Inr(), inr) => (r, bind(e, y -> rec(inr, e, s)), s)
    case v: Var[a]               => (CaseExpr(v.as,x,l,y,r), e, s)
    case arg                     => typeError(s"expected either (${Inl().show} _) or (${Inr().show} _) but got: ${arg.show}")

  case ( Let(x,t,u), e, s ) => rec(t, e, s) match

    case Application(Bang(), effect) =>
      val result = rec(effect, e, s) match
        case Lazy(thunk) => Pure(thunk())
        case result      => result
      (u, bind(e, x -> result), s)

    case v: Var[a] => (Let(x,v.as,u), e, s)
    case arg       => typeError(s"expected (! _) but got: ${arg.show}")

  case ( LetT(x,y,t,u), e, s ) => rec(t, e, s) match

    case Tensor(effect, next) =>
      val result = rec(effect, e, s) match
        case Lazy(thunk) => Pure(thunk())
        case result      => result
      (u, bind(e, x -> result, y -> next), s)

    case v: Var[a] => (LetT(x,y,v.as,u), e, s)
    case arg       => typeError(s"expected (! _ *: _) but got: ${arg.show}")

  end step

  private def run(s: State): State =
    println(s"step: ${s.show}")
    if s.finalState then s
    else run(step(s))

  private def rec(s: State): ErasedTree =
    run(s) match
    case (t, Nil, (m,f)::_) => throw TypeError(s"Tried to apply non-function $t to $m")
    case (t, _, _)          => t

  private def bind(env: Environment, bindings: (Name, ErasedTree)*): Environment =
    bindings.foldRight(env)((p, env) => Closure(p._1, p._2, Nil)::env)

  object Opaque
    def unapply(t: Pure[?] | Lazy[?] | Splice[?] | Point.type): true = true

  object Effect
    def unapply(t: Inl[?,?] | Inr[?,?] | Bang[?] | WhyNot[?]): true = true

  object Prod
    def unapply(t: Pair[?,?] | Tensor[?,?]): true = true

  object Binder
    def unapply(t: Lin[?,?] | Lam[?,?]): true = true

  object Lambda
    def unapply(t: Lin[?,?] | Lam[?,?]): (Name, ErasedTree) = Tuple.fromProduct(t).asInstanceOf

  object Application
    def unapply[A,B](t: ErasedTree): Option[(ErasedTree, ErasedTree)] = t match
    case t: App[t,u]  => Some(Tuple.fromProductTyped(t))
    case t: Eval[t,u] => Some(Tuple.fromProductTyped(t))
    case _            => None
    end unapply

  object Unreducible
    def unapply(t: ErasedTree): Boolean = t match
    case (
      Project(Var(_), _)       // can't reduce a var
    | Let(_,Var(_),_)          // can't reduce a var
    | LetT(_,_,Var(_),_)       // can't reduce a var
    | CaseExpr(Var(_),_,_,_,_) // can't reduce a var
    | Var(_)                   // can't reduce a var
    ) => true

    case _ => false

    end unapply

  object Closed
    def unapply(t: ErasedTree): Boolean = t match
    case (
      Opaque()                 // has no dependency
    | Application(Effect(), _) // effects require an enclosing expression to evaluate
    | Prod()                   // products require an enclosing expression to project
    ) => true

    case _ => false

    end unapply

  private def (s: State) finalState = s match
  case (Binder()     , _  , Nil) => true
  case (Unreducible(), Nil, _  ) => true
  case (Closed()     , _  , _  ) => true
  case _                         => false
  end finalState

  private def [T](e: ErasedTree) as: Tree[T] = e.asInstanceOf[Tree[T]]

  def run[T](t: Tree[T]): Unit =
    println(eval(t).merge)

  def reduce[T](t: Tree[T]): Either[TypeError, Tree[T]] =
    try Right(rec(start(t)).as)
    catch case e: TypeError => Left(e)

  def eval[T](t: Tree[T]): Either[TypeError, T] =
    reduce(t).flatMap {
      case Pure(t) => Right(t)
      case term    => Left(TypeError(s"Program terminated with ${term.show}"))
    }
