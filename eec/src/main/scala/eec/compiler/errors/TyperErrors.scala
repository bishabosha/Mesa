package eec
package compiler
package errors

object TyperErrors {
  
  enum TyperError {
    case Internal(e: Exception)
    case TypeError(msg: String)
  }

  object TyperError {
    import eec.util.Showable

    implicit val EECErrorShowable: Showable[TyperError] = new {
      override def (error: TyperError) userString: String = error match {
        case Internal(e: Exception) => e.getMessage
        case TypeError(msg: String) => msg
      }
    }
  }
}