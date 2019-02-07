package eec
package compiler
package error

object ParserErrors {
  
  enum ParserError {
    case Internal(e: Exception)
    case SyntaxError(msg: String)
  }

  object ParserError {

    import eec.util.Showable

    implicit val EECErrorShowable: Showable[ParserError] = new {
      override def (error: ParserError) userString: String = error match {
        case Internal(e: Exception) => e.getMessage
        case SyntaxError(msg: String) => msg
      }
    }
    
    def (f: => O) recover[O]: O | ParserError = {

      import scala.util.control._
      import parsers.ParserSyntaxException

      try {
        f
      } catch {
        case e: ParserSyntaxException => SyntaxError(e.getMessage)
        case e: Exception if NonFatal(e) => new Internal(e)
      }
    }
  }
}