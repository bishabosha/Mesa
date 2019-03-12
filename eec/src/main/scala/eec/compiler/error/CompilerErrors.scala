package eec
package compiler
package error

object CompilerErrors {

  import CompilerError._

  enum CompilerError derives Eql {
    case UnexpectedType(msg: String)
    case IllegalState(msg: String)
    case SyntaxError(msg: String)
    case Internal(e: Exception)
  }

  type Checked[O] = O | CompilerError

  object CompilerErrorOps {
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

    def (l: List[A]) mapE[A, O](f: A => Checked[O]): Checked[List[O]] = {
      import scala.collection._
      l.foldLeftE(new mutable.ListBuffer[O]) { (acc, a) =>
        for (o <- f(a)) yield {
          acc += o
          acc
        }
      }
      .map(_.toList)
    }

    /** shortcutting fold over a list - optimised */
    def (l: List[A]) foldLeftE[A, O](seed: O)(f: (O, A) => Checked[O]): Checked[O] = {
      var acc = seed
      var rest = l
      while (true) {
        rest match {
          case head :: tail =>
            f(acc, head) match {
              case e: CompilerError =>
                return e
              case t =>
                rest = tail
                acc = t.asInstanceOf[O]
            }
          case Nil =>
            return acc
        }
      }
      ???
    }

    import eec.util.Showable

    def (f: => O) recoverDefault[O]: Checked[O] = {
      import scala.util.control._
      import CompilerError._
      f.recover {
        case e: Exception if NonFatal(e) => Internal(e)
      }
    }

    def (f: => O) recover[O](
      opt: PartialFunction[Exception, CompilerError]): Checked[O] = {

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
      def (e: CompilerError) userString = e match {
        case Internal(e) =>
          val trace = e.getStackTraceString.split("\n")
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