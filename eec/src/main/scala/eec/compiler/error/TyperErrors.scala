package eec
package compiler
package error

object TyperErrors {
  
  enum TyperError {
    case Internal(e: Exception)
    case Typing(msg: String)
  }

  object TyperError {

    import eec.util.Showable

    type Typed[O] = O | TyperError

    def (o: Typed[O]) fold[O, U](e: TyperError => U)(f: O => U): U = o match {
      case err: TyperError => e(err)
      case otherwise => f(otherwise.asInstanceOf[O])
    }

    def (o: Typed[O]) map[O, U](f: O => U): Typed[U] = o.flatMap(f)

    def (o: Typed[O]) flatMap[O, U](f: O => Typed[U]): Typed[U] = o match {
      case e: TyperError => e
      case otherwise => f(otherwise.asInstanceOf[O])
    }

    def (l: List[A]) mapM[A](f: A => Typed[A]): Typed[List[A]] = {
      var rest = l
      var buffer: List[A] = Nil
      while (rest.nonEmpty) {
        f(rest.head) match {
          case e: TyperError => return e
          case t =>
            rest = rest.tail
            buffer = t.asInstanceOf[A] :: buffer
        }
      }
      buffer.reverse
    }

    implicit val EECErrorShowable: Showable[TyperError] = new {
      override def (error: TyperError) userString: String = error match {
        case Internal(e: Exception) => e.getMessage
        case Typing(msg: String) => msg
      }
    }
  }
}