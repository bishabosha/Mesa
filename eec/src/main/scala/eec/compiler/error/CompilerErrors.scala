package eec
package compiler
package error

object CompilerErrors {

  enum CompilerError derives Eql {
    case UnexpectedType(msg: String)
    case IllegalState(msg: String)
    case SyntaxError(msg: String)
    case Internal(e: Exception)
  }

  type Checked[O] = O | CompilerError

  object CompilerErrorOps {

    import eec.util.Showable
    import collection.generic.CanBuildFrom
    import collection.mutable.Builder
    import annotation.tailrec

    def checked[O](o: Checked[O]): Checked[O] = o

    def (o: Checked[O]) fold[O, U](e: CompilerError => U)(f: O => U): U =
      o match {
        case err: CompilerError => e(err)
        case _                  => f(o.asInstanceOf[O])
      }

    def (o: Checked[O]) map[O, U](f: O => U): Checked[U] =
      o match {
        case err: CompilerError => err
        case _                  => f(o.asInstanceOf[O])
      }

    def (o: Checked[O]) filter[O](f: O => Boolean)(orElse: O => CompilerError): Checked[O] =
      o match {
        case err: CompilerError =>
          err
        case _ =>
          if f(o.asInstanceOf[O]) then
            o
          else
            orElse(o.asInstanceOf[O])
      }

    def (o: Checked[O]) flatMap[O, U](f: O => Checked[U]): Checked[U] =
      o match {
        case err: CompilerError => err
        case _                  => f(o.asInstanceOf[O])
      }

    def (c: CC[A]) mapE[CC[A] <: Iterable[A], A, O, That](f: A => Checked[O])
        given (bf: CanBuildFrom[CC[A], O, That]): Checked[That] = {

      val b = bf(c)

      @tailrec
      def inner(it: Iterator[A]): Checked[That] =
        if it.hasNext then
          f(it.next) match {
            case err: CompilerError =>
              err
            case o =>
              b += o.asInstanceOf[O]
              inner(it)
          }
        else
          b.result

      inner(c.iterator)
    }

    def (l: Iterable[A]) foldLeftE[A, O](seed: O)
        (f: (O, A) => Checked[O]): Checked[O] = {

      @tailrec
      def inner(acc: O, it: Iterator[A]): Checked[O] =
        if it.hasNext then
          f(acc, it.next) match {
            case err: CompilerError => err
            case acc1               => inner(acc1.asInstanceOf[O], it)
          }
        else
          acc

      inner(seed, l.iterator)
    }

    def (f: => O) recoverDefault[O]: Checked[O] = {
      import scala.util.control._
      import CompilerError._
      f.recover {
        case e: Exception if NonFatal(e) => Internal(e)
      }
    }

    def (f: => O) recover[O]
        (opt: PartialFunction[Exception, CompilerError]): Checked[O] = {
      import scala.util.control._
      import CompilerError._

      try {
        f
      } catch {
        case e: Exception if opt.isDefinedAt(e) => opt(e)
        case e: Exception if NonFatal(e)        => Internal(e)
      }
    }

    implied for Showable[CompilerError] {
      import CompilerError._

      def (e: CompilerError) userString = e match {
        case Internal(e) =>
          val trace = e
            .getStackTraceString
            .split("\n")
            .toSeq
            .map("    " + _)
            .mkString("\n")
          s"Internal: ${e.getClass.getSimpleName}: ${e.getMessage}\nDebug trace:\n$trace"
        case UnexpectedType(msg)  => s"UnexpectedType: $msg"
        case IllegalState(msg)    => s"IllegalState: $msg"
        case SyntaxError(msg)     => s"SyntaxError: $msg"
      }
    }
  }
}