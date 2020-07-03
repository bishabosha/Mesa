package mesa.eec

import Trees._, Tree._
import deriving.Mirror
import mesa.util.Show
import annotation.tailrec

final class TypeError(msg: String) extends RuntimeException(msg)

object Kernel:
  import Continuation._

  case class Env(x: Name, t: ErasedTree, e: Environment)
  case class Stp(x: Name, t: ErasedTree, e: Stoup)

  enum Continuation:
    case Apply(e: Environment, arg: ErasedTree)
    case Evaluate(s: Stoup, e: Environment, arg: ErasedTree)
    case Pi(i: Int)
    case Sigma(e: Environment, inl: Name, l: ErasedTree, inr: Name, r: ErasedTree)
    case Effect(e: Environment, x: Name, u: ErasedTree)
    case UnZip(e: Environment, x: Name, y: Name, u: ErasedTree)

  given as Show[Continuation] = { case p: Product => p.productPrefix.toLowerCase }

  type Environment = List[Env]
  type Stoup       = Option[Stp]
  type Constraint  = Boolean
  type State       = (ErasedTree, Stoup, Environment, Constraint, List[Continuation])
  final val Z      = None
  final val C      = true
  final val V      = false

  private inline def (s: State).continuation: List[Continuation] = s(4)

  given (using m: Mirror.ProductOf[Env], rec: => Show[m.MirroredElemTypes]) as Show[Env] =
    Tuple.fromProductTyped(_).show

  given (using m: Mirror.ProductOf[Stp], rec: => Show[m.MirroredElemTypes]) as Show[Stp] =
    Tuple.fromProductTyped(_).show

  private def typeError(msg: String): Nothing = throw TypeError(msg)

  private extension on (s: State):

    def betaReduce: State = (s: @unchecked) match
      case ( Pair(x,_),      s, e, d, Pi(0)            :: ks ) => ( x, s,           e,                     d, ks )
      case ( Pair(_,y),      s, e, d, Pi(1)            :: ks ) => ( y, s,           e,                     d, ks )
      case ( Inl(t),         s, e, d, Sigma(f,l,u,_,_) :: ks ) => ( u, s,           bind(f,e,l,t),         d, ks )
      case ( Inr(t),         s, e, d, Sigma(f,_,_,r,u) :: ks ) => ( u, s,           bind(f,e,r,t),         d, ks )
      case ( t @ Lazy(_),    s, e, C, Effect(f,x,u)    :: ks ) => ( u, Z,           bind(f,e,x,t.compute), C, ks )
      case ( Bang(t),        Z, e, C, Effect(f,x,u)    :: ks ) => ( u, Z,           bind(f,e,x,t),         C, ks )
      case ( Tensor(t,c),    s, e, C, UnZip(f,x,y,u)   :: ks ) => ( u, bind(s,y,c), bind(f,e,x,t),         C, ks )
      case ( Lam(x,t),       s, e, d, Apply(f,m)       :: ks ) => ( t, s,           bind(e,f,x,m),         d, ks )
      case ( Lin(x,t),       Z, _, V, Evaluate(s,f,m)  :: ks ) => ( t, bind(s,x,m), f,                     C, ks )
      case ( err @ Bang(_),  _, _, _, (_: Effect)      :: _  ) => typeError(s"unexpected ${err.show} in linear context")
      case ( err @ Lin(_,_), _, _, _, (_: Evaluate)    :: _  ) => typeError(s"unexpected ${err.show} in linear context")
      case ( err,            _, _, _, (_: Pi)          :: _  ) => typeError(s"expected (_,_) but got: ${err.show}")
      case ( err,            _, _, _, (_: Sigma)       :: _  ) => typeError(s"expected (inl _) or (inr _) but got: ${err.show}")
      case ( err,            _, _, _, (_: Effect)      :: _  ) => typeError(s"expected (!_) but got: ${err.show}")
      case ( err,            _, _, _, (_: UnZip)       :: _  ) => typeError(s"expected (!_*:_) but got: ${err.show}")
      case ( err,            _, _, _, (_: Apply)       :: _  ) => typeError(s"expected (\\_._) but got ${err.show}")
      case ( err,            _, _, _, (_: Evaluate)    :: _  ) => typeError(s"expected (^\\_._) but got ${err.show}")

    def step: State = (s: @unchecked) match
      case ( Var(x),              Some(Stp(y,n,f)), e,                  C, ks ) if x == y => ( n, f, e, C,                     ks )
      case ( Var(x),              Z,                     Env(y,n,f)::_, V, ks ) if x == y => ( n, Z, f, V,                     ks )
      case ( v @ Var(_),          Z,                     Env(_,_,_)::e, V, ks )           => ( v, Z, e, V,                     ks )
      case ( App(t,m),            s,                     e,             d, ks )           => ( t, s, e, d, Apply(e,m)       :: ks )
      case ( Eval(t,m),           s,                     e,             _, ks )           => ( t, s, e, V, Evaluate(s,e,m)  :: ks )
      case ( Fst(t),              s,                     e,             d, ks )           => ( t, s, e, d, Pi(0)            :: ks )
      case ( Snd(t),              s,                     e,             d, ks )           => ( t, s, e, d, Pi(1)            :: ks )
      case ( CaseExpr(t,l,u,r,v), s,                     e,             d, ks )           => ( t, s, e, d, Sigma(e,l,u,r,v) :: ks )
      case ( Let(x,t,u),          s,                     e,             _, ks )           => ( t, s, e, C, Effect(e,x,u)    :: ks )
      case ( LetT(x,y,t,u),       s,                     e,             _, ks )           => ( t, s, e, C, UnZip(e,x,y,u)   :: ks )
      case ( Var(x),              Some(_),               Env(y,n,_)::_, _, _  ) if x == y => typeError(s"Illegal reference to ${n.show} in linear context")
      case ( Var(_),              Some(_),               _,             V, _  )           => typeError(s"evaluating linear value in pure context")
      case ( Var(x),              Z,                     _,             C, _  )           => typeError(s"evaluating unbound value $x in computational context")

  @tailrec
  private def run(state: State, debug: Boolean): ErasedTree =
    if debug then
      println(s"step: ${Show.show(state)}")
    if state.redexOrFinal then
      if state.continuation.isEmpty then
        state.head
      else
        run(state.betaReduce, debug)
    else
      run(state.step, debug)
  end run

  private def bind(env: Environment, outer: Environment, name: Name, value: ErasedTree): Environment =
    if name == "_" then env else Env(name, value, outer)::env

  private def bind(outer: Stoup, name: Name, value: ErasedTree): Stoup =
    Some(Stp(name, value, outer))

  object Redex:
    def unapply(redex:
        Pure[?]
      | Splice[?]
      | Lam[?,?]
      | Lin[?,?]
      | Inl[?,?]
      | Inr[?,?]
      | Bang[?]
      | Lazy[?]
      | WhyNot[?]
      | Pair[?,?]
      | Tensor[?,?]
      | Point.type): true = true

  private def (s: State).redexOrFinal = s match
    case (Redex(), _,    _  , _, _ ) => true
    case (Var(_),  None, Nil, _, _ ) => true
    case _                           => false

  def reduce[T](t: Tree[T], debug: Boolean = false): Either[TypeError, Tree[T]] =
    try Right(run(state = (t, Z, Nil, V, Nil), debug).as)
    catch case e: TypeError => Left(e)

  def eval[T](t: Tree[T], debug: Boolean = false): Either[TypeError, T] =
    reduce(t, debug).flatMap {
      case Pure(t) => Right(t)
      case Point   => Right(())
      case term    => Left(TypeError(s"Program terminated with ${term.show}"))
    }
