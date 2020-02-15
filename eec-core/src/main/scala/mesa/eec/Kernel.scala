package mesa.eec

import Trees._, Tree._
import deriving.Mirror
import mesa.util.Show
import annotation.tailrec

final class TypeError(msg: String) extends RuntimeException(msg)

object Kernel:
  import Continuation._

  case class Closure(x: Name, t: ErasedTree, e: Environment)

  enum Continuation:
    case Apply(e: Environment, arg: ErasedTree)
    case Evaluate(e: Environment, arg: ErasedTree)
    case Pi(e: Environment, i: Int)
    case Sigma(e: Environment, inl: Name, l: ErasedTree, inr: Name, r: ErasedTree)
    case Bind(e: Environment, x: Name, u: ErasedTree)
    case Reduce(e: Environment, x: Name, u: ErasedTree)
    case UnZip(e: Environment, x: Name, y: Name, u: ErasedTree)
    case ReduceLeft(e: Environment, x: Name, y: Name, yVal: ErasedTree, u: ErasedTree)

  given Show[Continuation] = { case p: Product => p.productPrefix.toLowerCase }

  type Environment = List[Closure]
  type State       = (ErasedTree, Environment, List[Continuation]) // TODO: add stoup with correct semantics

  private inline def (s: State).continuation: List[Continuation] = s(2)

  given (given m: Mirror.ProductOf[Closure], rec: => Show[m.MirroredElemTypes]): Show[Closure] =
    Tuple.fromProductTyped(_).show

  private def typeError(msg: String): Nothing = throw TypeError(msg)

  private extension on (s: State):

    def betaReduce: State = (s: @unchecked) match
      case ( Pair(x,_),   _, Pi(e,0)               :: ks ) => ( x, e, ks )
      case ( Pair(_,y),   _, Pi(e,1)               :: ks ) => ( y, e, ks )
      case ( Inl(t),      _, Sigma(e,l,u,_,_)      :: ks ) => ( u, bind(e,l->t), ks )
      case ( Inr(t),      _, Sigma(e,_,_,r,u)      :: ks ) => ( u, bind(e,r->t), ks )
      case ( t,           _, Reduce(e,x,u)         :: ks ) => ( u, bind(e,x->t), ks )
      case ( t,           _, ReduceLeft(e,x,y,s,u) :: ks ) => ( u, bind(e,x->t,y->s), ks )
      case ( t @ Lazy(_), _, Bind(e,x,u)           :: ks ) => ( u, bind(e,x->t.compute), ks )
      case ( Bang(t),     e, Bind(f,x,u)           :: ks ) => ( t, e, Reduce(f,x,u)::ks )
      case ( Tensor(t,s), e, UnZip(f,x,y,u)        :: ks ) => ( t, e, ReduceLeft(f,x,y,s,u)::ks )
      case ( Lam(x,t),    e, Apply(f,m)            :: ks ) => ( t, Closure(x,m,f)::e, ks )
      case ( Lin(x,t),    e, Evaluate(f,m)         :: ks ) => ( t, Closure(x,m,f)::e, ks )
      case ( err,         _, (_: Pi)               :: ks ) => typeError(s"expected (_,_) but got: ${err.show}")
      case ( err,         _, (_: Sigma)            :: ks ) => typeError(s"expected (inl _) or (inr _) but got: ${err.show}")
      case ( err,         _, (_: Bind)             :: ks ) => typeError(s"expected (!_) but got: ${err.show}")
      case ( err,         _, (_: UnZip)            :: ks ) => typeError(s"expected (!_*:_) but got: ${err.show}")
      case ( err,         _, (_: Apply)            :: ks ) => typeError(s"expected (\\_._) but got ${err.show}")
      case ( err,         _, (_: Evaluate)         :: ks ) => typeError(s"expected (^\\_._) but got ${err.show}")

    def step: State = (s: @unchecked) match
      case ( v @ Var(x),          Closure(y,n,f)::e, ks ) => if x == y then (n,f,ks) else (v,e,ks)
      case ( App(t,m),            e,                 ks ) => (t, e, Apply(e,m)       :: ks)
      case ( Eval(t,m),           e,                 ks ) => (t, e, Evaluate(e,m)    :: ks)
      case ( Fst(t),              e,                 ks ) => (t, e, Pi(e,0)          :: ks)
      case ( Snd(t),              e,                 ks ) => (t, e, Pi(e,1)          :: ks)
      case ( CaseExpr(t,l,u,r,v), e,                 ks ) => (t, e, Sigma(e,l,u,r,v) :: ks)
      case ( Let(x,t,u),          e,                 ks ) => (t, e, Bind(e,x,u)      :: ks)
      case ( LetT(x,y,t,u),       e,                 ks ) => (t, e, UnZip(e,x,y,u)   :: ks)

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

  private def bind(env: Environment, bindings: (Name, ErasedTree)*): Environment =
    bindings.foldRight(env)((p, env) => if p.head == "_" then env else Closure(p(0), p(1), Nil)::env)

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
  case (Redex(), _  , _ ) => true
  case (Var(_) , Nil, _ ) => true
  case _                  => false

  def reduce[T](t: Tree[T], debug: Boolean = false): Either[TypeError, Tree[T]] =
    try Right(run(state = (t, Nil, Nil), debug).as)
    catch case e: TypeError => Left(e)

  def eval[T](t: Tree[T], debug: Boolean = false): Either[TypeError, T] =
    reduce(t, debug).flatMap {
      case Pure(t) => Right(t)
      case term    => Left(TypeError(s"Program terminated with ${term.show}"))
    }
