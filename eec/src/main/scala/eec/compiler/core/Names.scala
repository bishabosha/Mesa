package eec
package compiler
package core

import types.Types._
import Type._
import Contexts.Id
import util.{Show, Read}

object Names {
  import Name._
  import Derived._

  import implied NameOps._
  import implied DerivedOps._

  enum Derived derives Eql {
    case Str(str: String)
    case Synthetic(id: Id, str: String)
  }

  enum Name derives Eql {
    case From(derived: Derived)
    case Comp(derived: Derived)
    case BangTag, TensorTag, IntegerTag, DecimalTag, VoidTag, VoidCompTag,
      BooleanTag, StringTag, CharTag
    case Wildcard
    case EmptyName
  }

  val rootName: Name = "_root_".readAs

  object DerivedOps {
    implied for Show[Derived] = {
      case Str(str)           => str
      case Synthetic(id, str) => s"<$str:$id>"
    }

    implied for Read[Derived] = Str(_)
  }

  object NameOps {

    def (name: Name) nonEmpty = name != EmptyName

    def (name: Name) foldEmptyName[O, U](empty: => O)(f: Name => U): O | U =
      if name == EmptyName then empty
      else f(name)

    def (name: Name) foldWildcard[O, U](wildcard: => O)(f: Name => U): O | U =
      if name == Wildcard then wildcard
      else f(name)

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

    implied for Show[Name] = {
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

    implied for Read[Name] = {
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
      case str        => From(the[Read[Derived]].readAs(str))
    }
  }
}