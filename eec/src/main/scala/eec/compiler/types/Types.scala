package eec
package compiler
package types

import scala.collection.generic.CanBuild
import scala.annotation.tailrec

import ast.Trees._
import Tree._
import core.Names
import Names._
import Name._
import util.{Showable,|>,StackMachine}
import StackMachine._
import Program._

import implied NameOps._

object Types {
  import Type._
  import TypeOps._
  import TypeVariableOps._

  object Bootstraps {
    val BooleanType = TypeRef(BooleanTag)
    val DecimalType = TypeRef(DecimalTag)
    val IntegerType = TypeRef(IntegerTag)
    val CharType    = TypeRef(CharTag)
    val StringType  = TypeRef(StringTag)
  }

  enum Type derives Eql {
    case PackageInfo(parent: Type, name: Name)
    case TypeRef(name: Name)
    case Variable(name: Name)
    case FunctionType(arg: Type, body: Type)
    case Product(args: List[Type])
    case AppliedType(typ: Type, args: List[Type])
    case WildcardType
    case Untyped
    case EmptyType
  }

  opaque type TypeVariableOps = Type

  private object TypeVariableOps {
    def apply(tpe: Type): TypeVariableOps = tpe

    def (ops: TypeVariableOps) zipWith[O, That](subWith: Type)
        (f: (Name, Type) => O)
        given (bf: CanBuild[O, That]): That = {

      val b = ops.zipFold(subWith)(bf()) { (acc, arg, app) =>
        arg match {
          case Variable(name) => acc += f(name, app)
          case _              => acc
        }
      }

      b.result
    }

    def (tpe: TypeVariableOps) mapVariables(f: Name => Type): Type = {
      tpe.mapLeaves {
        case Variable(name) => f(name)
        case tpe1           => tpe1
      }
    }

    def unifyImpl(tpe: Type, sub: Name, by: Type): Type = tpe.mapVariables {
      case `sub` =>
        if by == WildcardType then
          Variable(sub)
        else
          by

      case other => Variable(other)
    }
  }

  object TypeOps {

    def (tpe: Type) mapTypeRefs(f: Name => Type): Type = {
      tpe.mapLeaves {
        case TypeRef(name)  => f(name)
        case tpe1           => tpe1
      }
    }

    private[Types] def (tpe: Type) compute[O]
                       (compiler: Type => Statement[O]): O = {
      tpe.compile[Type, O](foldLeft)(compiler)
    }

    private[Types] def (tpe: Type) mapLeaves(f: Type => Type): Type = {
      tpe.compute {
        case FunctionType(arg, body) =>
          stack =>
            val a1 :: a2 :: rest = stack
            FunctionType(a1, a2) :: rest

        case AppliedType(f, args) =>
          stack =>
            val functor :: stack1 = stack
            val (removed, rest) = stack1.splitAt(args.length)
            AppliedType(functor, removed) :: rest

        case Product(tpes1) =>
          stack =>
            val (removed, rest) = stack.splitAt(tpes1.length)
            Product(removed) :: rest

        case tpe1 => f(tpe1) :: _
      }
    }

    def (tpe: Type) foldLeft[O]
        (z: O)
        (f: (O, Type) => O): O = {
      @tailrec
      def inner(z: O, tpes: List[Type]): O = tpes match {
        case Nil => z

        case tpe :: rest =>
          tpe match {
            case AppliedType(t, tpes) => inner(f(z, tpe), t :: tpes ::: rest)
            case FunctionType(a1, b1) => inner(f(z, tpe), a1 :: b1 :: rest)
            case Product(tpes)        => inner(f(z, tpe), tpes ::: rest)
            case _                    => inner(f(z, tpe), rest)
          }
      }
      inner(z, tpe :: Nil)
    }

    def (subFrom: Type) zipFold [O, That]
        (subWith: Type)
        (z: O)
        (f: (O, Type, Type) => O): O = {

      @tailrec
      def inner(z: O, args: List[Type], apps: List[Type]): O = args match {
        case Nil => z

        case arg :: argRest => apps match {
          case Nil => z

          case app :: appsRest =>
            if canUnify(arg, app) then (arg, app) match {
              case (AppliedType(fun1, tpes1), AppliedType(fun2, tpes2)) =>
                inner(
                  f(z, arg, app),
                  fun1 :: tpes1 ::: argRest,
                  fun2 :: tpes2 ::: appsRest
                )

              case (FunctionType(a1, b1), FunctionType(a2, b2)) =>
                inner(
                  f(z, arg, app),
                  a1 :: b1 :: argRest,
                  a2 :: b2 :: appsRest
                )

              case (Product(tpes1), Product(tpes2)) =>
                inner(
                  f(z, arg, app),
                  tpes1 ::: argRest,
                  tpes2 ::: appsRest
                )

              case _ => inner(f(z, arg, app), argRest, appsRest)
            } else {
              z
            }
        }
      }

      if WildcardType == subWith || WildcardType == subFrom then z
      else inner(z, subFrom :: Nil, subWith :: Nil)
    }

    def canUnify(arg: Type, app: Type): Boolean = (arg, app) match {
      case (Product(tpes1), Product(tpes2)) => tpes1.size == tpes2.size
      case (Variable(_:Comp), _)            => app.isComputationType

      case (AppliedType(_, args1), AppliedType(_, args2)) =>
        args1.size == args2.size

      case ( _:FunctionType,   _:FunctionType )
      |    ( _:TypeRef,        _:TypeRef      )
      |    ( _:Variable
           |   WildcardType,   _              )
      |    ( _:PackageInfo,    _:PackageInfo  )
      |    (   EmptyType,        EmptyType    )
      |    (   Untyped,          Untyped      ) => true
      case _                                    => false
    }

    def (tpe: Type) =!= (other: Type): Boolean = {
      tpe   == other        ||
      tpe   == WildcardType ||
      other == WildcardType
    }

    def packageName(tpe: Type): Name = tpe match {
      case PackageInfo(_, name) => name
      case _                    => EmptyName
    }

    def packageNames(tpe: Type): List[Name] = {
      @tailrec
      def inner(acc: List[Name], tpe: Type): List[Name] = tpe match {
        case PackageInfo(parent, name)  => inner(name :: acc, parent)
        case _                          => acc
      }
      inner(Nil, tpe)
    }

    def toCurriedList(t: Type): List[Type] = {
      @tailrec
      def inner(acc: List[Type], t: Type): List[Type] = t match {
        case FunctionType(arg, body)  => inner(arg :: acc, body)
        case _                        => t :: acc
      }
      inner(Nil, t).reverse
    }

    def toReturnType(defSig: Tree, t: Type): Type =
      defSig match {
        case DefSig(_, args) =>
          toFunctionType(toCurriedList(t).drop(args.length))

        case _ => EmptyType
      }

    def toFunctionType(ts: List[Type]): Type = ts.reverse match {
      case t :: rest  => rest.foldLeft(t)((acc, t1) => FunctionType(t1, acc))
      case Nil        => EmptyType
    }

    def (subFrom: Type) unifications(subWith: Type): List[(Name, Type)] =
      TypeVariableOps(subFrom).zipWith(subWith)((_, _))

    def (tpe: Type) unifyFrom(subFrom: Type)(subWith: Type): Type = {
      subFrom.zipFold(subWith)(tpe) { (acc, arg, sub) =>
        arg match {
          case Variable(name) => unifyImpl(acc, name, sub)
          case _              => acc
        }
      }
    }

    inline def (tpe: Type) unify(subWith: Type) =
      tpe.unifyFrom(tpe)(subWith)

    def (tpe: Type) unifyFromAll(unifications: Iterable[(Name, Type)]): Type = {
      unifications.foldLeft(tpe) { (acc, pair) =>
        val (sub, by) = pair
        unifyImpl(acc, sub, by)
      }
    }

    def (tpe: Type) replaceVariable(from: Name)(by: Name): Type =
      unifyImpl(tpe, from, Variable(by))

    def (tpe: Type) replaceVariables(f: Name => Option[Name]): Type = {
      var seen = Set[Name]()
      tpe.foldLeft(tpe) { (acc, tpe) =>
        tpe match {
          case Variable(n) if !seen.contains(n) =>
            seen += n
            f(n).fold(acc)(acc.replaceVariable(n)(_))

          case _ => acc
        }
      }
    }

    def (tpe: Type) isComputationType: Boolean = {
      @tailrec
      def inner(acc: Boolean, tpes: List[Type]): Boolean = tpes match {
        case Nil => acc

        case tpe :: tpes => tpe match {
          case AppliedType(TypeRef(ComputationTag), List(_))
          |    TypeRef(_: Comp)
          |    Variable(_: Comp)      => true
          case FunctionType(_, body)  => inner(acc, body :: tpes)
          case Product(ts)            => inner(acc, ts ::: tpes)
          case _                      => false
        }
      }
      inner(true, tpe :: Nil)
    }

    def (tpe: Type) isValueType = !tpe.isComputationType

    implied for Showable[Type] {

      def (tpe: Type) show: String = tpe.compute {
        case FunctionType(arg, body)    => fromFunctionType(arg, body)
        case AppliedType(f, args)       => fromAppliedType(f, args)
        case Product(tpes1)             => fromProduct(tpes1)
        case TypeRef(t)                 => t.show :: _
        case Variable(t)                => t.show :: _
        case PackageInfo(parent, name)  => showPackage(parent, name) :: _
        case WildcardType               => "<anytype>" :: _
        case Untyped                    => "<untyped>" :: _
        case EmptyType                  => "<emptytype>" :: _
      }

      private def fromFunctionType(arg: Type, body: Type)
                                  (stack: Stack[String]) = {
        val a1 :: a2 :: rest = stack
        val a1Final = arg match {
          case _: FunctionType  => s"($a1)"
          case _                => a1
        }
        s"$a1Final -> $a2" :: rest
      }

      private def fromAppliedType(f: Type, args: List[Type])
                                 (stack: Stack[String]) = {
        def checkAll(tpes: List[Type], strs: List[String]) =
          tpes.zip(strs).map(check)

        def check(tpe: Type, str: String) = tpe match {
          case _: (FunctionType | AppliedType) =>
            s"($str)"
          case _ =>
            str
        }

        val functor :: stack1 = stack
        val (removed, rest) = stack1.splitAt(args.length)
        val str = {
          if removed.isEmpty then {
            functor
          } else {
            val init =
              if f == TypeRef(ComputationTag) && args.length == 1 then functor
              else s"$functor "
            checkAll(args, removed).mkString(init, " ", "")
          }
        }
        str :: rest
      }

      private def fromProduct(tpes: List[Type])
                             (stack: Stack[String]) = {
        val (removed, rest) = stack.splitAt(tpes.length)
        removed.mkString("(", ", ", ")") :: rest
      }

      private inline def showPackage(parent: Type, name: Name): String = {
        (packageNames(parent) ::: name :: Nil)
          .map(_.show)
          .mkString("package ", ".", "")
      }
    }

    implied for (List[Type] |> Type) {
      def apply(ts: List[Type]) = ts match {
        case tpe :: Nil => tpe
        case types      => Product(types)
      }
    }

    implied for (Type |> List[Type]) {
      def apply(t: Type) = t match {
        case Product(ls)  => ls
        case tpe          => tpe :: Nil
      }
    }
  }
}