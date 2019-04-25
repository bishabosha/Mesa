package eec
package compiler
package core

import types.Types._
import Type._
import Contexts.Id
import util.{Show, Read, Define}

object Names {
  import Name._

  import implied NameOps._

  enum Name derives Eql {
    case From(derived: String)
    case Comp(derived: String)
    case BangTag, TensorTag, IntegerTag, DecimalTag, VoidTag, VoidCompTag,
      BooleanTag, StringTag, CharTag
    case Wildcard
    case EmptyName
  }

  val rootName: Name = "_root_".readAs

  object NameOps {

    private[Names] val OpId = """([!#/%&*+-:<=>?@\\^|~]+)""".r

    def (name: Name) isOperator = name match {
      case From(OpId(_)) => true
      case Comp(OpId(_)) => true
      case _             => false
    }

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

    implied for Show[Name] = {
      case Comp(s)        => s
      case From(s)        => s
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

    implied for Define[Name] = {
      case Comp(OpId(op)) => s"($op)"
      case From(OpId(op)) => s"($op)"
      case Comp(s)        => s
      case From(s)        => s
      case BangTag        => s"(${BangTag.show})"
      case TensorTag      => s"(${TensorTag.show})"
      case other          => other.show
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
      case str        => From(str)
    }
  }
}