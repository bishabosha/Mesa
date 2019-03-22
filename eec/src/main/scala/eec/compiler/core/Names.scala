package eec
package compiler
package core

object Names {

  import Name._
  import types.Types._
  import implied NameOps._
  import Type._
  import Contexts.Id

  enum Derived derives Eql {
    case Str(str: String)
    case Synthetic(id: Id, str: String)
  }

  enum Name derives Eql {
    case From(derived: Derived)
    case Comp(derived: Derived)
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

  object DerivedOps {
    import Derived._
    import eec.util.{Showable, Readable}

    implied for Showable[Derived] {
      def (n: Derived) show = n match {
        case Str(str) => str
        case Synthetic(id, str) => s"<$str:$id>"
      }
    }

    implied for Readable[Derived] {
      def (s: String) readAs = Str(s)
    }
  }

  object NameOps {
    import Name._
    import Derived._
    import implied DerivedOps._
    import eec.util.{Showable, Readable}

    def (name: Name) promoteComp: Name = name match {
      case From(d)  => Comp(d)
      case _        => name
    }

    def (name: Name) updateDerivedStr(f: String => Derived): Option[Name] =
      name match {
        case From(Str(str)) => Some(From(f(str)))
        case Comp(Str(str)) => Some(Comp(f(str)))
        case _              => None
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
        case Comp(n)        => n.show
        case From(n)        => n.show
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
        case str        => From(infer[Readable[Derived]].readAs(str))
      }
    }
  }
}