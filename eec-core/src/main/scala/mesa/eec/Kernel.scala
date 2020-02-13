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

  private def start[T](t: Tree[T]): State = (t, Nil, Nil)

  private def typeError(msg: String): Nothing = throw TypeError(msg)

  private extension on (s: State):

    def stepPi: State = (s: @unchecked) match
    case ( Pair(x,_), _, Pi(e,0) :: ks ) => (x, e, ks)
    case ( Pair(_,y), _, Pi(e,1) :: ks ) => (y, e, ks)
    case ( err      , _, Pi(_,i) :: _  ) => typeError(s"expected (_,_) as argument to ${List("fst","snd")(i)} but got: ${err.show}")

    def stepSigma: State = (s: @unchecked) match
    case ( Inl(inl), _, Sigma(e,x,l,_,_) :: ks ) => (l, bind(e, x -> inl), ks)
    case ( Inr(inr), _, Sigma(e,_,_,y,r) :: ks ) => (r, bind(e, y -> inr), ks)
    case ( err     , _, Sigma(_,_,_,_,_) :: _  ) => typeError(s"expected either (inl _) or (inr _) but got: ${err.show}")

    def stepBind: State = (s: @unchecked) match
    case ( Bang(value), e, Bind(f,x,u) :: ks ) => (value, e                       , Reduce(f,x,u) :: ks)
    case ( l @ Lazy(_), e, Bind(f,x,u) :: ks ) => (u    , bind(e, x -> compute(l)), ks                 )
    case ( err        , _, Bind(_,_,_) :: _  ) => typeError(s"expected (!_) but got: ${err.show}")

    def stepReduce: State = (s: @unchecked) match
    case ( result, _, Reduce(e,x,u) :: ks ) => (u, bind(e, x -> result), ks)

    def stepUnZip: State = (s: @unchecked) match
    case ( Tensor(value, next), e, UnZip(f,x,y,u) :: ks ) => (value, e, ReduceLeft(f,x,y,next,u) :: ks)
    case ( err                , _, UnZip(_,_,_,_) :: _  ) => typeError(s"expected (!_*:_) but got: ${err.show}")

    def stepReduceLeft: State = (s: @unchecked) match
    case ( result, _, ReduceLeft(e,x,y,next,u) :: ks ) => (u, bind(e, x -> result, y -> next) , ks)

    def stepApply: State = (s: @unchecked) match
    case ( Lambda(x,t), e, Apply(f,m) :: ks ) => (t, Closure(x,m,f)::e, ks )
    case ( err        , _, Apply(_,m) :: ks ) => throw TypeError(s"Tried to apply non-function ${err.show} to ${m.show}")

    def stepRoot: State = (s: @unchecked) match
    case ( v @ Var(x)         , Closure(y,n,f)::e, ks ) => if x == y then (n,f,ks) else (v,e,ks)
    case ( Application(n,m)   , e                , ks ) => (n, e, Apply(e,m)       :: ks)
    case ( Project(p,i)       , e                , ks ) => (p, e, Pi(e,i)          :: ks)
    case ( CaseExpr(p,x,l,y,r), e                , ks ) => (p, e, Sigma(e,x,l,y,r) :: ks)
    case ( Let(x,t,u)         , e                , ks ) => (t, e, Bind(e,x,u)      :: ks)
    case ( LetT(x,y,t,u)      , e                , ks ) => (t, e, UnZip(e,x,y,u)   :: ks)
    end stepRoot

  @tailrec private def run(s: State): ErasedTree =
    println(s"step: ${Show.show(s)}")
    if s.finalState then s.continuation match
      case (_: Pi)         :: _ => run(s.stepPi)
      case (_: Apply)      :: _ => run(s.stepApply)
      case (_: Sigma)      :: _ => run(s.stepSigma)
      case (_: Bind)       :: _ => run(s.stepBind)
      case (_: Reduce)     :: _ => run(s.stepReduce)
      case (_: UnZip)      :: _ => run(s.stepUnZip)
      case (_: ReduceLeft) :: _ => run(s.stepReduceLeft)
      case Nil                  => s.head
    else
      run(s.stepRoot)
  end run

  private def bind(env: Environment, bindings: (Name, ErasedTree)*): Environment =
    bindings.foldRight(env)((p, env) => if p.head == "_" then env else Closure(p(0), p(1), Nil)::env)

  object Project:
    def unapply(t: Fst[?,?] | Snd[?,?]): (ErasedTree, Int) = t match
    case Fst(p) => (p, 0)
    case Snd(p) => (p, 1)

  object Opaque:
    def unapply(t: Pure[?] | Lazy[?] | Splice[?] | Point.type): true = true

  object Effect:
    def unapply(t: Inl[?,?] | Inr[?,?] | Bang[?] | WhyNot[?]): true = true

  object Prod:
    def unapply(t: Pair[?,?] | Tensor[?,?]): true = true

  object Binder:
    def unapply(t: Lin[?,?] | Lam[?,?]): true = true

  object Lambda:
    def unapply(t: Lin[?,?] | Lam[?,?]): (Name, ErasedTree) = Tuple.fromProduct(t).asInstanceOf

  object Application:
    def unapply(t: ErasedTree): Option[(ErasedTree, ErasedTree)] = t match
    case t: App[t,u]  => Some(Tuple.fromProductTyped(t))
    case t: Eval[t,u] => Some(Tuple.fromProductTyped(t))
    case _            => None
    end unapply

  object Closed:
    def unapply(t: ErasedTree): Boolean = t match
    case (
      Opaque() // has no dependency
    | Effect() // effects require an enclosing expression to evaluate
    | Prod()   // products require an enclosing expression to project
    | Binder() // binder requires enclosing application to reduce
    ) => true
    case _ => false
    end unapply

  private def (s: State).finalState = s match
  case (Closed(), _  , _  ) => true
  case (Var(_)  , Nil, _  ) => true
  case _                    => false
  end finalState

  def reduce[T](t: Tree[T]): Either[TypeError, Tree[T]] =
    try Right(run(start(t)).as)
    catch case e: TypeError => Left(e)

  def eval[T](t: Tree[T]): Either[TypeError, T] =
    reduce(t).flatMap {
      case Pure(t) => Right(t)
      case term    => Left(TypeError(s"Program terminated with ${term.show}"))
    }
