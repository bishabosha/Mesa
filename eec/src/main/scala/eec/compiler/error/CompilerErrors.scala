package eec
package compiler
package error

import scala.annotation.tailrec
import scala.collection.generic.CanBuildFrom
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

    given as Show[CompilerError] = {
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

    def (o: Lifted[O]) onError [O]
        (handler: CompilerError => Nothing): O = o match {
      case err: CompilerError => handler(err)
      case _                  => unlift(o)
    }

    def (o: Lifted[O]) doOnError [O]
        (handler: CompilerError => Unit): Unit = o match {
      case err: CompilerError => handler(err)
      case _                  =>
    }

    inline def unlift[O](o: Lifted[O]): O = o.asInstanceOf[O]

    def (o: Lifted[O]) fold [O, U]
        (e: CompilerError => U)
        (f: O => U): U = o match {
      case err: CompilerError => e(err)
      case _                  => f(unlift(o))
    }

    def (o: Lifted[O]) map [O, U] (f: O => U): Lifted[U] = o match {
      case err: CompilerError => err
      case _                  => f(unlift(o))
    }

    def (o: Lifted[O]) foreach [O, U] (f: O => Unit): Lifted[Unit] = o match {
      case err: CompilerError => err
      case _                  => f(unlift(o))
    }

    def (o: Lifted[O]) flatMap [O, U]
        (f: O => Lifted[U]): Lifted[U] = o match {
      case err: CompilerError => err
      case _                  => f(unlift(o))
    }

    def (c: Option[A]) mapE [A, O]
        (f: A => Lifted[O]): Lifted[Option[O]] = {
      c.fold[Lifted[Option[O]]](None){
        f(_) match {
          case err: CompilerError => err
          case o                  => Some(unlift(o))
        }
      }
    }

    def (c: CC[A]) mapE [CC[A] <: Iterable[A], A, O, That]
        (f: A => Lifted[O])
        given (bf: CanBuildFrom[CC[A], O, That]): Lifted[That] = {
      @tailrec
      def inner(acc: mutable.Builder[O, That], it: Iterator[A]): Lifted[That] = {
        if it.hasNext then f(it.next) match {
          case err: CompilerError => err
          case o                  => inner(acc += unlift(o), it)
        } else {
          acc.result
        }
      }

      inner(bf(c), c.iterator)
    }

    def (l: Iterable[A]) foldLeftE[A, O]
        (z: O)
        (f: (O, A) => Lifted[O]): Lifted[O] = {

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

    def (f: => O) recover[O]
        (opt: PartialFunction[Throwable, CompilerError]): Lifted[O] = {
      try {
        f
      } catch {
        case NonFatal(e) if opt.isDefinedAt(e) => opt(e)
      }
    }
  }
}