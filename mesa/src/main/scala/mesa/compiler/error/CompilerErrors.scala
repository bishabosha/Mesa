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

  enum CompilerError derives CanEqual {
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

    extension [O](o: Lifted[O]) def onError(handler: CompilerError => Nothing): O = o match {
      case err: CompilerError => handler(err)
      case _                  => unlift(o)
    }

    extension [O](o: Lifted[O]) def doOnError(handler: CompilerError => Unit): Unit = o match {
      case err: CompilerError => handler(err)
      case _                  =>
    }

    inline def unlift[O](o: Lifted[O]): O = o.asInstanceOf[O]

    extension [O, U](o: Lifted[O]) def fold(e: CompilerError => U)(f: O => U): U = o match {
      case err: CompilerError => e(err)
      case _                  => f(unlift(o))
    }

    extension [O, U](o: Lifted[O]) def map(f: O => U): Lifted[U] = o match {
      case err: CompilerError => err
      case _                  => f(unlift(o))
    }

    extension [O, U](o: Lifted[O]) def foreach(f: O => Unit): Lifted[Unit] = o match {
      case err: CompilerError => err
      case _                  => f(unlift(o))
    }

    extension [O, U](o: Lifted[O]) def flatMap(f: O => Lifted[U]): Lifted[U] = o match {
      case err: CompilerError => err
      case _                  => f(unlift(o))
    }

    extension [A, O](c: Option[A]) def mapE(f: A => Lifted[O]): Lifted[Option[O]] = c.fold[Lifted[Option[O]]](None) {
      f(_) match {
        case err: CompilerError => err
        case o                  => Some(unlift(o))
      }
    }

    extension [CC[A] <: Iterable[A], A, O](c: CC[A]) def mapE(f: A => Lifted[O])(using factory: Factory[O, CC[O]]): Lifted[CC[O]] = {

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

    extension [A, O](l: Iterable[A]) def foldLeftE(z: O)(f: (O, A) => Lifted[O]): Lifted[O] = {

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

    extension [O](f: => O) def recover(opt: PartialFunction[Throwable, CompilerError]): Lifted[O] = {
      try {
        f
      } catch {
        case NonFatal(e) if opt.isDefinedAt(e) => opt(e)
      }
    }
  }
}
