package eec
package compiler
package object exception {
  type EECError = Internal | TypeError | SyntaxError

  def recover[O](f: => O): O | EECError = {
    import scala.util.control._
    try {
      f
    } catch {
      case e: EECError => e
      case e: Exception if NonFatal(e) => new Internal(e)
    }
  }
}