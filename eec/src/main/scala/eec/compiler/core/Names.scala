package eec
package compiler
package core

import types.Types._
import Type._
import Contexts.Id
import util.{Showable, Readable}

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
    case ComputationTag, IntegerTag, DecimalTag, EitherTag, VoidTag,
      VoidCompTag, BooleanTag, StringTag, CharTag, Wildcard, TensorTag,
      CoTensorTag
    case EmptyName
  }

  val rootName: Name = "_root_".readAs

  val tensorConstructor = {
    FunctionType(
      AppliedType(
        TypeRef("!".readAs),
        List(Variable("$A".readAs))
      ),
      FunctionType(
        Variable("$B#".readAs),
        InfixAppliedType(
          TypeRef(TensorTag),
          AppliedType(
            TypeRef("!".readAs),
            List(Variable("$A".readAs))
          ),
          Variable("$B#".readAs)
        )
      )
    )
  }

  val coTensorConstructor = {
    FunctionType(
      Variable("$A#".readAs),
      FunctionType(
        Variable("$B#".readAs),
        InfixAppliedType(
          TypeRef(CoTensorTag),
          Variable("$A#".readAs),
          Variable("$B#".readAs)
        )
      )
    )
  }

  val bangConstructor = {
    FunctionType(
      Variable("$v".readAs),
      AppliedType(
        TypeRef("!".readAs),
        List(Variable("$v".readAs))
      )
    )
  }

  val eitherConstructor = {
    FunctionType(
      Variable("$l".readAs),
      FunctionType(
        Variable("$r".readAs),
        AppliedType(
          TypeRef(EitherTag),
          List(
            Variable("$l".readAs),
            Variable("$r".readAs)
          )
        )
      )
    )
  }

  val bootstrapped = {
    List(
      TensorTag       -> tensorConstructor,
      CoTensorTag     -> coTensorConstructor,
      ComputationTag  -> bangConstructor,
      EitherTag       -> eitherConstructor,
      VoidCompTag     -> TypeRef(VoidCompTag),
      VoidTag         -> TypeRef(VoidTag),
      IntegerTag      -> TypeRef(IntegerTag),
      DecimalTag      -> TypeRef(DecimalTag),
      BooleanTag      -> TypeRef(BooleanTag),
      StringTag       -> TypeRef(StringTag),
      CharTag         -> TypeRef(CharTag)
    )
  }

  object DerivedOps {
    implied for Showable[Derived] {
      def (n: Derived) show = n match {
        case Str(str)           => str
        case Synthetic(id, str) => s"<$str:$id>"
      }
    }

    implied for Readable[Derived] {
      def (s: String) readAs = Str(s)
    }
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

    implied for Showable[Name] {
      def (n: Name) show = n match {
        case Comp(n)        => n.show
        case From(n)        => n.show
        case ComputationTag => "!"
        case TensorTag      => "|*|"
        case CoTensorTag    => "|+|"
        case EitherTag      => "Either"
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
    }

    implied for Readable[Name] {
      def (str: String) readAs = str match {
        case "!"        => ComputationTag
        case "|*|"      => TensorTag
        case "|+|"      => CoTensorTag
        case "_"        => Wildcard
        case "Integer"  => IntegerTag
        case "Decimal"  => DecimalTag
        case "Boolean"  => BooleanTag
        case "String"   => StringTag
        case "Char"     => CharTag
        case "Either"   => EitherTag
        case "Void"     => VoidTag
        case "Void#"    => VoidCompTag
        case _          => From(infer[Readable[Derived]].readAs(str))
      }
    }
  }
}