package mesa
package compiler
package core

import types.Types._
import Type._
import Contexts.Id
import util.{Show, Read, Define}

object Names {
  import Name._
  import Derived._
  import DerivedOps._

  import NameOps.given
  import DerivedOps.given

  enum Derived derives CanEqual {
    case Str(str: String)
    case Synthetic(id: Id, str: String)
  }

  enum Name derives CanEqual {
    case From(derived: Derived)
    case Comp(derived: Derived)
    case BangTag, TensorTag, IntegerTag, DecimalTag, VoidTag, VoidCompTag,
      BooleanTag, StringTag, CharTag
    case Wildcard
    case EmptyName
  }

  val rootName: Name = "_root_".readAs

  object DerivedOps {
    private[Names] val OpId = """([!#/%&*+-:<=>?@\\^|~]+)""".r

    given Define[Derived] = {
      case Str(OpId(str))     => s"($str)"
      case Str(str)           => str
      case Synthetic(id, str) => s"<$str:$id>"
    }

    given Show[Derived] = {
      case Str(str)           => str
      case Synthetic(id, str) => s"<$str:$id>"
    }

    given Read[Derived] = Str(_)
  }

  object NameOps {

    extension (name: Name) def isOperator = name match {
      case From(Str(OpId(_))) | From(Synthetic(_, OpId(_))) => true
      case Comp(Str(OpId(_))) | Comp(Synthetic(_, OpId(_))) => true
      case _                                                => false
    }

    extension (name: Name) def nonEmpty = name != EmptyName

    extension [O, U](name: Name) def foldEmptyName(empty: => O)(f: Name => U): O | U =
      if name == EmptyName then empty
      else f(name)

    extension [O, U](name: Name) def foldWildcard(wildcard: => O)(f: Name => U): O | U =
      if name == Wildcard then wildcard
      else f(name)

    extension (name: Name) def promoteComp: Name = name match {
      case From(d)  => Comp(d)
      case _        => name
    }

    extension (name: Name) def updateDerivedStr(f: String => Derived): Option[Name] =
      name match {
        case From(Str(str)) => Some(From(f(str)))
        case Comp(Str(str)) => Some(Comp(f(str)))
        case _              => None
      }

    given Show[Name] = {
      case Comp(n)        => n.show
      case From(n)        => n.show
      case BangTag        => "!"
      case TensorTag      => "*:"
      case VoidTag        => "Void"
      case VoidCompTag    => "Void#"
      case IntegerTag     => "Integer"
      case DecimalTag     => "Decimal"
      case BooleanTag     => "Boolean"
      case StringTag      => "String"
      case CharTag        => "Char"
      case Wildcard       => "_"
      case EmptyName      => "<empty>"
    }

    given Define[Name] = {
      case Comp(n)    => n.define
      case From(n)    => n.define
      case BangTag    => s"(${BangTag.show})"
      case TensorTag  => s"(${TensorTag.show})"
      case other      => other.show
    }

    given Read[Name] = {
      case "!"        => BangTag
      case "*:"       => TensorTag
      case "_"        => Wildcard
      case "Integer"  => IntegerTag
      case "Decimal"  => DecimalTag
      case "Boolean"  => BooleanTag
      case "String"   => StringTag
      case "Char"     => CharTag
      case "Void"     => VoidTag
      case "Void#"    => VoidCompTag
      case str        => From(summon[Read[Derived]].readAs(str))
    }
  }
}
