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

    def stepPi: State = (s: @unchecked) match
    case ( Pair(x,_), _, Pi(e,0) :: ks ) => (x, e, ks)
    case ( Pair(_,y), _, Pi(e,1) :: ks ) => (y, e, ks)
    case ( err      , _, _             ) => typeError(s"expected (_,_) but got: ${err.show}")

    def stepSigma: State = (s: @unchecked) match
    case ( Inl(inl), _, Sigma(e,x,l,_,_) :: ks ) => (l, bind(e, x -> inl), ks)
    case ( Inr(inr), _, Sigma(e,_,_,y,r) :: ks ) => (r, bind(e, y -> inr), ks)
    case ( err     , _, _                      ) => typeError(s"expected either (inl _) or (inr _) but got: ${err.show}")

    def stepBind: State = (s: @unchecked) match
    case ( Bang(value), e, Bind(f,x,u) :: ks ) => (value, e                       , Reduce(f,x,u) :: ks)
    case ( l @ Lazy(_), e, Bind(f,x,u) :: ks ) => (u    , bind(e, x -> compute(l)), ks                 )
    case ( err        , _, _                 ) => typeError(s"expected (!_) but got: ${err.show}")

    def stepReduce: State = (s: @unchecked) match
    case ( result, _, Reduce(e,x,u) :: ks ) => (u, bind(e, x -> result), ks)

    def stepUnZip: State = (s: @unchecked) match
    case ( Tensor(value, next), e, UnZip(f,x,y,u) :: ks ) => (value, e, ReduceLeft(f,x,y,next,u) :: ks)
    case ( err                , _, _                    ) => typeError(s"expected (!_*:_) but got: ${err.show}")

    def stepReduceLeft: State = (s: @unchecked) match
    case ( result, _, ReduceLeft(e,x,y,next,u) :: ks ) => (u, bind(e, x -> result, y -> next) , ks)

    def stepApply: State = (s: @unchecked) match
    case ( Lam(x,t), e, Apply(f,m) :: ks ) => (t, Closure(x,m,f)::e, ks )
    case ( err        , _, _             ) => typeError(s"expected (\\_._) but got ${err.show}")

    def stepEvaluate: State = (s: @unchecked) match
    case ( Lin(x,t), e, Evaluate(f,m) :: ks ) => (t, Closure(x,m,f)::e, ks )
    case ( err        , _, _                ) => typeError(s"expected (^\\_._) but got ${err.show}")

    def stepRoot: State = (s: @unchecked) match
    case ( v @ Var(x)         , Closure(y,n,f)::e, ks ) => if x == y then (n,f,ks) else (v,e,ks)
    case ( App(n,m)           , e                , ks ) => (n, e, Apply(e,m)       :: ks)
    case ( Eval(n,m)          , e                , ks ) => (n, e, Evaluate(e,m)    :: ks)
    case ( Project(p,i)       , e                , ks ) => (p, e, Pi(e,i)          :: ks)
    case ( CaseExpr(p,x,l,y,r), e                , ks ) => (p, e, Sigma(e,x,l,y,r) :: ks)
    case ( Let(x,t,u)         , e                , ks ) => (t, e, Bind(e,x,u)      :: ks)
    case ( LetT(x,y,t,u)      , e                , ks ) => (t, e, UnZip(e,x,y,u)   :: ks)
    end stepRoot

  @tailrec
  private def run(state: State, debug: Boolean): ErasedTree =
    if debug then println(s"step: ${Show.show(state)}")
    if state.finalState then state.continuation match
      case (_: Pi)         :: _ => run(state.stepPi, debug)
      case (_: Apply)      :: _ => run(state.stepApply, debug)
      case (_: Evaluate)   :: _ => run(state.stepEvaluate, debug)
      case (_: Sigma)      :: _ => run(state.stepSigma, debug)
      case (_: Bind)       :: _ => run(state.stepBind, debug)
      case (_: Reduce)     :: _ => run(state.stepReduce, debug)
      case (_: UnZip)      :: _ => run(state.stepUnZip, debug)
      case (_: ReduceLeft) :: _ => run(state.stepReduceLeft, debug)
      case Nil                  => state.head
    else
      run(state.stepRoot, debug)
  end run

  private def bind(env: Environment, bindings: (Name, ErasedTree)*): Environment =
    bindings.foldRight(env)((p, env) => if p.head == "_" then env else Closure(p(0), p(1), Nil)::env)

  object Project:
    def unapply(t: Fst[?,?] | Snd[?,?]): (ErasedTree, Int) = t match
    case Fst(p) => (p, 0)
    case Snd(p) => (p, 1)

  object Opaque: // has no dependency
    def unapply(t: Pure[?] | Lazy[?] | Splice[?] | Point.type): true = true

  object Effect: // effects require an enclosing expression to evaluate
    def unapply(t: Inl[?,?] | Inr[?,?] | Bang[?] | WhyNot[?]): true = true

  object Prod: // products require an enclosing expression to project
    def unapply(t: Pair[?,?] | Tensor[?,?]): true = true

  object Binder: // binder requires enclosing application to reduce
    def unapply(t: Lin[?,?] | Lam[?,?]): true = true

  object Closed:
    def unapply(t: ErasedTree): Boolean = t match
    case (Opaque() | Effect() | Prod() | Binder()) => true
    case _                                         => false

  private def (s: State).finalState = s match
  case (Closed(), _  , _  ) => true
  case (Var(_)  , Nil, _  ) => true
  case _                    => false

  def reduce[T](t: Tree[T], debug: Boolean = false): Either[TypeError, Tree[T]] =
    try Right(run(state = (t, Nil, Nil), debug).as)
    catch case e: TypeError => Left(e)

  def eval[T](t: Tree[T], debug: Boolean = false): Either[TypeError, T] =
    reduce(t, debug).flatMap {
      case Pure(t) => Right(t)
      case term    => Left(TypeError(s"Program terminated with ${term.show}"))
    }
