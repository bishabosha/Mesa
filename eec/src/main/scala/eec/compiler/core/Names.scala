package eec
package compiler
package core

object Names {

  import Name._
  import types.Types._
  import Type._

  enum Name derives Eql {
    case From(name: String)
    case ComputationTag, IntegerTag, DecimalTag,
      BooleanTag, StringTag, CharTag, Wildcard, EmptyName
  }

  val emptyString: String = "<empty>"
  val rootString: String  = "_root_"

  val bootstrapped = List(
    ComputationTag  -> FunctionType(Generic(From("a")), AppliedType(TypeRef(ComputationTag), List(Generic(From("a"))))),
    IntegerTag      -> TypeRef(IntegerTag),
    DecimalTag      -> TypeRef(DecimalTag),
    BooleanTag      -> TypeRef(BooleanTag),
    StringTag       -> TypeRef(StringTag),
    CharTag         -> TypeRef(CharTag)
  )

  object NameOps {

    import Name._
    import eec.util.{Showable, Readable}

    implied for Showable[Name] {
      def (n: Name) userString = n match {
        case Wildcard       => "_"
        case ComputationTag => "!"
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
      def (s: String) readAs = s match {
        case "_"        => Wildcard
        case "!"        => ComputationTag
        case "Integer"  => IntegerTag
        case "Decimal"  => DecimalTag
        case "Boolean"  => BooleanTag
        case "String"   => StringTag
        case "Char"     => CharTag
        case str        => From(str)
      }
    }
  }
}