package mesa
package compiler
package error

import scala.annotation.tailrec
import scala.collection.Factory
import scala.collection.mutable
import scala.util.control.NonFatal

import util.Show

object CompilerErrors {
  import CompilerError._
  import CompilerErrorOps._

  type Lifted[O] = O | CompilerError

  enum CompilerError derives Eql {
    case NameCollision(msg: String)
    case LinearScope(msg: String)
    case UnknownIdentifier(msg: String)
    case UnexpectedType(msg: String)
    case MissingCase(msg: String)
    case IllegalState(msg: String)
    case Internal(msg: String)
    case IllegalInput(msg: String)
    case Syntax(msg: String)
  }

  object CompilerErrorOps {

    given Show[CompilerError] = {
      case e @ NameCollision(msg)     => s"${e.productPrefix}: $msg"
      case e @ LinearScope(msg)       => s"${e.productPrefix}: $msg"
      case e @ UnknownIdentifier(msg) => s"${e.productPrefix}: $msg"
      case e @ UnexpectedType(msg)    => s"${e.productPrefix}: $msg"
      case e @ MissingCase(msg)       => s"${e.productPrefix}: $msg"
      case e @ IllegalState(msg)      => s"${e.productPrefix}: $msg"
      case e @ Internal(msg)          => s"${e.productPrefix}: $msg"
      case e @ IllegalInput(msg)      => s"${e.productPrefix}: $msg"
      case e @ Syntax(msg)            => s"${e.productPrefix}: $msg"
    }

    def lift[O](o: Lifted[O]): Lifted[O] = o

    def [O](o: Lifted[O]) onError(handler: CompilerError => Nothing): O = o match {
      case err: CompilerError => handler(err)
      case _                  => unlift(o)
    }

    def [O](o: Lifted[O]) doOnError(handler: CompilerError => Unit): Unit = o match {
      case err: CompilerError => handler(err)
      case _                  =>
    }

    inline def unlift[O](o: Lifted[O]): O = o.asInstanceOf[O]

    def [O, U](o: Lifted[O]) fold(e: CompilerError => U)(f: O => U): U = o match {
      case err: CompilerError => e(err)
      case _                  => f(unlift(o))
    }

    def [O, U](o: Lifted[O]) map(f: O => U): Lifted[U] = o match {
      case err: CompilerError => err
      case _                  => f(unlift(o))
    }

    def [O, U](o: Lifted[O]) foreach(f: O => Unit): Lifted[Unit] = o match {
      case err: CompilerError => err
      case _                  => f(unlift(o))
    }

    def [O, U](o: Lifted[O]) flatMap(f: O => Lifted[U]): Lifted[U] = o match {
      case err: CompilerError => err
      case _                  => f(unlift(o))
    }

    def [A, O](c: Option[A]) mapE(f: A => Lifted[O]): Lifted[Option[O]] = c.fold[Lifted[Option[O]]](None) {
      f(_) match {
        case err: CompilerError => err
        case o                  => Some(unlift(o))
      }
    }

    def [CC[A] <: Iterable[A], A, O](c: CC[A]) mapE(f: A => Lifted[O])(given factory: Factory[O, CC[O]]): Lifted[CC[O]] = {

      @tailrec
      def inner(acc: mutable.Builder[O, CC[O]], it: Iterator[A]): Lifted[CC[O]] = {
        if it.hasNext then f(it.next) match {
          case err: CompilerError => err
          case o                  => inner(acc += unlift(o), it)
        } else {
          acc.result
        }
      }

      inner(factory.newBuilder, c.iterator)
    }

    def [A, O](l: Iterable[A]) foldLeftE(z: O)(f: (O, A) => Lifted[O]): Lifted[O] = {

      @tailrec
      def inner(acc: O, it: Iterator[A]): Lifted[O] =
        if it.hasNext then
          f(acc, it.next) match {
            case err: CompilerError => err
            case acc1               => inner(unlift(acc1), it)
          }
        else
          acc

      inner(z, l.iterator)
    }

    def [O](f: => O) recover(opt: PartialFunction[Throwable, CompilerError]): Lifted[O] = {
      try {
        f
      } catch {
        case NonFatal(e) if opt.isDefinedAt(e) => opt(e)
      }
    }
  }
}
