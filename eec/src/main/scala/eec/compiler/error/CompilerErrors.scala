package eec
package compiler
package error

object CompilerErrors {
  
  enum CompilerError {
    case UnexpectedType(msg: String)
    case IllegalState(msg: String)
    case SyntaxError(msg: String)
    case Internal(e: Exception)
  }

  type Checked[O] = O | CompilerError

  object CompilerErrorOps {
    def (o: Checked[O]) fold[O, U](e: CompilerError => U)(f: O => U): U =
      o match {
        case err: CompilerError => e(err)
        case _                  => f(o.asInstanceOf[O])
      }

    def (o: Checked[O]) map[O, U](f: O => U): Checked[U] = o.flatMap(f)

    def (o: Checked[O]) flatMap[O, U](f: O => Checked[U]): Checked[U] =
      o match {
        case e: CompilerError => e
        case _                => f(o.asInstanceOf[O])
      }

    def (l: List[A]) flatMapM[A, O](f: A => Checked[O]): Checked[List[O]] = {
      import scala.collection._
      var rest = l
      var buffer = new mutable.ListBuffer[O]
      while (rest.nonEmpty) {
        f(rest.head) match {
          case e: CompilerError => return e
          case t                =>
            rest = rest.tail
            buffer += t.asInstanceOf[O]
        }
      }
      buffer.toList
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
      import CompilerError._
      def (error: CompilerError) userString: String = error match {
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