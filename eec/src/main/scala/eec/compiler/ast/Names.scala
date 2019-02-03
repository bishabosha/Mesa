package eec
package compiler
package ast

object Names {

  enum Name {
    case Empty
    case Wildcard
    case From(name: String)
    case Bootstrapped(b: Bootstraps)
  }

  enum Bootstraps {
    case ComputationTag, UnitTag, Tuple2Tag, IntegerTag, DecimalTag, BooleanTag, StringTag, CharTag
  }

  object NameOps {
    import Name._
    import BootstrapsOps._

    def (str: String) asName: Name = str match {
      case "<empty>" => Empty
      case "_" => Wildcard
      case n => n.eecToBootstraps match {
        case Some(b) => Bootstrapped(b)
        case _ => From(n)
      }
    }

    def (name: Name) stringName = name match {
      case Empty => "<empty>"
      case Wildcard => "_"
      case Bootstrapped(b) => b.asEec
      case From(n) => n
    }
  }

  object BootstrapsOps {
    import Bootstraps._

    def (bootstraps: Bootstraps) asEec = bootstraps match {
      case ComputationTag => "!"
      case UnitTag => "()"
      case Tuple2Tag => "(,)"
      case IntegerTag => "Integer"
      case DecimalTag => "Decimal"
      case BooleanTag => "Boolean"
      case StringTag => "String"
      case CharTag => "Char"
    }

    def (str: String) eecToBootstraps: Option[Bootstraps] = str match {
      case "!" => Some(ComputationTag)
      case "()" => Some(UnitTag)
      case "(,)" => Some(Tuple2Tag)
      case "Integer" => Some(IntegerTag)
      case "Decimal" => Some(DecimalTag)
      case "Boolean" => Some(BooleanTag)
      case "String" => Some(StringTag)
      case "Char" => Some(CharTag)
      case _ => None
    }
  }

}