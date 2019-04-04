package eec
package compiler
package error

import scala.annotation.tailrec
import scala.collection.generic.CanBuildFrom
import scala.collection.mutable
import scala.util.control.NonFatal

import util.Showable

object CompilerErrors {
  import CompilerError._
  import CompilerErrorOps._

  enum CompilerError derives Eql {
    case UnexpectedType(msg: String)
    case IllegalState(msg: String)
    case SyntaxError(msg: String)
    case Internal(e: Exception)
  }

  type Checked[O] = O | CompilerError

  object CompilerErrorOps {

    def checked[O](o: Checked[O]): Checked[O] = o

    def (o: Checked[O]) onError [O]
        (handler: CompilerError => Nothing): O = o match {
      case err: CompilerError => handler(err)
      case _                  => unchecked(o)
    }

    inline def unchecked[O](o: Checked[O]): O = o.asInstanceOf[O]

    def (o: Checked[O]) fold [O, U]
        (e: CompilerError => U)
        (f: O => U): U = o match {
      case err: CompilerError => e(err)
      case _                  => f(unchecked(o))
    }

    def (o: Checked[O]) map [O, U] (f: O => U): Checked[U] = o match {
      case err: CompilerError => err
      case _                  => f(unchecked(o))
    }

    def (o: Checked[O]) flatMap [O, U]
        (f: O => Checked[U]): Checked[U] = o match {
      case err: CompilerError => err
      case _                  => f(unchecked(o))
    }

    def (c: CC[A]) mapE [CC[A] <: Iterable[A], A, O, That]
        (f: A => Checked[O])
        given (bf: CanBuildFrom[CC[A], O, That]): Checked[That] = {
      @tailrec
      def inner(acc: mutable.Builder[O, That], it: Iterator[A]): Checked[That] = {
        if it.hasNext then f(it.next) match {
          case err: CompilerError => err
          case o                  => inner(acc += unchecked(o), it)
        } else {
          acc.result
        }
      }

      inner(bf(c), c.iterator)
    }

    def (l: Iterable[A]) foldLeftE[A, O]
        (z: O)
        (f: (O, A) => Checked[O]): Checked[O] = {

      @tailrec
      def inner(acc: O, it: Iterator[A]): Checked[O] =
        if it.hasNext then
          f(acc, it.next) match {
            case err: CompilerError => err
            case acc1               => inner(unchecked(acc1), it)
          }
        else
          acc

      inner(z, l.iterator)
    }

    def (f: => O) recover[O]
        (opt: PartialFunction[Exception, CompilerError]): Checked[O] = {
      try {
        f
      } catch {
        case e: Exception if opt.isDefinedAt(e) => opt(e)
        case e: Exception if NonFatal(e)        => Internal(e)
      }
    }

    implied for Showable[CompilerError] {
      def (e: CompilerError) show = e match {
        case Internal(error) =>
          val trace = {
            error
              .getStackTraceString
              .split("\n")
              .toSeq
              .map("    " + _)
              .mkString("\n")
          }
          s"Internal: ${e.getClass.getSimpleName}: ${error.getMessage}\nDebug trace:\n$trace"

        case UnexpectedType(msg)  => s"UnexpectedType: $msg"
        case IllegalState(msg)    => s"IllegalState: $msg"
        case SyntaxError(msg)     => s"SyntaxError: $msg"
      }
    }
  }
}