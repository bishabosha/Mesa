package eec
package compiler
package core

object Names {

  import Name._
  import types.Types._
  import Type._

  enum Name {
    case From(name: String)
    case ComputationTag, UnitTag, Tuple2Tag, IntegerTag, DecimalTag,
      BooleanTag, StringTag, CharTag, Wildcard, EmptyName
  }

  val emptyString: String = "<empty>"
  val rootString: String  = "_root_"

  val bootstrapped = List(
    (ComputationTag, FunctionType(WildcardType, AppliedType(TypeRef(ComputationTag), List(WildcardType)))),
    (IntegerTag, TypeRef(IntegerTag)),
    (DecimalTag, TypeRef(DecimalTag)),
    (BooleanTag, TypeRef(BooleanTag)),
    (StringTag, TypeRef(StringTag)),
    (CharTag, TypeRef(CharTag))
  )

  object NameOps {

    import Name._
    import eec.util.{Showable, Readable}

    implied for Showable[Name] {
      def (name: Name) userString: String = name match {
        case Wildcard       => "_"
        case ComputationTag => "!"
        case UnitTag        => "()"
        case Tuple2Tag      => "(,)"
        case IntegerTag     => "Integer"
        case DecimalTag     => "Decimal"
        case BooleanTag     => "Boolean"
        case StringTag      => "String"
        case CharTag        => "Char"
        case EmptyName      => emptyString
        case From(n)        => n
      }
    }

    implied for Readable[Name] {
      def (str: String) readAs: Name = str match {
        case "_"        => Wildcard
        case "!"        => ComputationTag
        case "()"       => UnitTag
        case "(,)"      => Tuple2Tag
        case "Integer"  => IntegerTag
        case "Decimal"  => DecimalTag
        case "Boolean"  => BooleanTag
        case "String"   => StringTag
        case "Char"     => CharTag
        case _          => From(str)
      }
    }
  }
}