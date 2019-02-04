package eec
package compiler
package types

object Types {

  enum Type {
    case EmptyType
  }

  object Type {
    import Type._
    import eec.util.Showable

    implicit val TypeShowable: Showable[Type] = new {
      override def (typ: Type) userString: String = typ match {
        case EmptyType       => "Void"
      }
    }
  }

}