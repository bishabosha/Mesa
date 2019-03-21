package eec
package compiler
package core

object Names {

  import Name._
  import types.Types._
  import implied NameOps._
  import Type._

  enum Name derives Eql {
    case From(name: String)
    case Comp(name: String)
    case ComputationTag, IntegerTag, DecimalTag, EitherTag,
      BooleanTag, StringTag, CharTag, Wildcard, EmptyName
  }

  val emptyString: String = "<empty>"
  val rootString: String  = "_root_"

  val bootstrapped = List(
    ComputationTag  -> FunctionType(Variable("$v".readAs), AppliedType(TypeRef("!".readAs), List(Variable("$v".readAs)))),
    EitherTag       -> FunctionType(Variable("$l".readAs), FunctionType(Variable("$r".readAs), AppliedType(TypeRef(EitherTag), List(Variable("$l".readAs), Variable("$r".readAs))))),
    IntegerTag      -> TypeRef(IntegerTag),
    DecimalTag      -> TypeRef(DecimalTag),
    BooleanTag      -> TypeRef(BooleanTag),
    StringTag       -> TypeRef(StringTag),
    CharTag         -> TypeRef(CharTag)
  )

  object NameOps {

    import Name._
    import eec.util.{Showable, Readable}

    def (name: Name) promoteComp: Name = name match {
      case From(str)  => Comp(str)
      case _          => name
    }

    implied for Showable[Name] {
      def (n: Name) show = n match {
        case Wildcard       => "_"
        case ComputationTag => "!"
        case IntegerTag     => "Integer"
        case DecimalTag     => "Decimal"
        case BooleanTag     => "Boolean"
        case StringTag      => "String"
        case CharTag        => "Char"
        case EitherTag      => "Either"
        case EmptyName      => emptyString
        case Comp(n)        => n
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
        case "Either"   => EitherTag
        case str        => From(str)
      }
    }
  }
}