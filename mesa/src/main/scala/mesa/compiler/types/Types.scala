package mesa.compiler.types

import scala.language.implicitConversions

import scala.collection.Factory
import scala.collection.BuildFrom
import scala.annotation.tailrec

import mesa.compiler.ast.Trees._
import Tree._
import mesa.compiler.core.Names
import Names._
import Name._
import mesa.compiler.error.CompilerErrors._
import CompilerErrorOps._
import mesa.util.{Show, Define, StackMachine, view, const}
import StackMachine._
import Program._

import NameOps.given

object Types {
  import Type._
  import TypeOps._

  enum Type derives Eql {
    case PackageInfo(parent: Type, name: Name)
    case TypeRef(name: Name)
    case Variable(name: Name)
    case FunctionType(arg: Type, body: Type)
    case LinearFunctionType(arg: Type, body: Type)
    case Product(args: List[Type])
    case BaseType(name: Name)
    case AppliedType(op: Name, args: List[Type])
    case InfixAppliedType(op: Name, a1: Type, a2: Type)
    case WildcardType
    case EmptyType
  }

  object Bootstraps {
    val BooleanType   = BaseType(BooleanTag)
    val DecimalType   = BaseType(DecimalTag)
    val IntegerType   = BaseType(IntegerTag)
    val CharType      = BaseType(CharTag)
    val StringType    = BaseType(StringTag)
    val VoidType      = BaseType(VoidTag)
    val VoidCompType  = BaseType(VoidCompTag)
    val UnitType      = Product(Nil)

    val TensorType = {
      InfixAppliedType(
        TensorTag,
        Variable("A#".readAs),
        Variable("B#".readAs)
      )
    }

    val BangType = {
      AppliedType(
        BangTag,
        List(Variable("A".readAs))
      )
    }
  }

  val bootstrapped = {
    IArray(
      TensorTag   -> Bootstraps.TensorType,
      BangTag     -> Bootstraps.BangType,
      VoidCompTag -> Bootstraps.VoidCompType,
      VoidTag     -> Bootstraps.VoidType,
      IntegerTag  -> Bootstraps.IntegerType,
      DecimalTag  -> Bootstraps.DecimalType,
      BooleanTag  -> Bootstraps.BooleanType,
      StringTag   -> Bootstraps.StringType,
      CharTag     -> Bootstraps.CharType
    )
  }

  object TypeOps {

    def unifyImpl(tpe: Type, sub: Name, by: Type): Type = {
      val replacement = {
        if by == WildcardType then Variable(_)
        else const(by)
      }
      tpe.mapVariables {
        case `sub` => replacement(sub)
        case other => Variable(other)
      }
    }

    def (tpe: Type) mapTypeRefs(f: Name => Type): Type = {
      tpe.mapLeaves {
        case TypeRef(name)  => f(name)
        case tpe1           => tpe1
      }
    }

    given Interpretable[Type] {
      def [O](tpe: Type) interpret(z: O)(f: (O, Type) => O): O = tpe.foldLeft(z)(f)
    }

    def (tpe: Type) mapVariables(f: Name => Type): Type = {
      tpe.mapLeaves {
        case Variable(name) => f(name)
        case tpe1           => tpe1
      }
    }

    private[Types] def (tpe: Type) mapLeaves(f: Type => Type): Type = {
      tpe.compile[Type, Type] {
        case FunctionType(arg, body) =>
          val a1 :: a2 :: rest = stack
          FunctionType(a1, a2) :: rest

        case LinearFunctionType(arg, body) =>
          val a1 :: a2 :: rest = stack
          LinearFunctionType(a1, a2) :: rest

        case InfixAppliedType(tpe, a1, a2) =>
          val a1 :: a2 :: rest = stack
          InfixAppliedType(tpe, a1, a2) :: rest

        case AppliedType(f, args) =>
          val (removed, rest) = stack.splitAt(args.length)
          AppliedType(f, removed) :: rest

        case Product(tpes1) =>
          val (removed, rest) = stack.splitAt(tpes1.length)
          Product(removed) :: rest

        case tpe1 => f(tpe1) :: stack
      }
    }

    def [O](tpe: Type) foldLeft(z: O)(f: (O, Type) => O): O = {
      @tailrec
      def inner(z: O, tpes: List[Type]): O = tpes match {
        case Nil => z

        case tpe :: rest =>
          tpe match {
            case AppliedType(_, tpes) => inner(f(z, tpe), tpes ::: rest)
            case FunctionType(a1, b1) => inner(f(z, tpe), a1 :: b1 :: rest)

            case LinearFunctionType(a1, b1) =>
              inner(f(z, tpe), a1 :: b1 :: rest)

            case InfixAppliedType(_, a1, a2) =>
              inner(f(z, tpe), a1 :: a2 :: rest)

            case Product(tpes) => inner(f(z, tpe), tpes ::: rest)
            case _             => inner(f(z, tpe), rest)
          }
      }
      inner(z, tpe :: Nil)
    }

    def [O](tpe: Type) foldLeftLifted(z: Lifted[O])(f: (O, Type) => Lifted[O]): Lifted[O] = {
      @tailrec
      def inner(z: Lifted[O], tpes: List[Type]): Lifted[O] = {
        z match {
          case err: CompilerError => err
          case z0 =>
            val z = unlift(z0)
            tpes match {
              case Nil => z

              case tpe :: rest =>
                tpe match {
                  case AppliedType(_, tpes) => inner(f(z, tpe), tpes ::: rest)
                  case FunctionType(a1, b1) => inner(f(z, tpe), a1 :: b1 :: rest)

                  case LinearFunctionType(a1, b1) =>
                    inner(f(z, tpe), a1 :: b1 :: rest)

                  case InfixAppliedType(_, a1, a2) =>
                    inner(f(z, tpe), a1 :: a2 :: rest)

                  case Product(tpes) => inner(f(z, tpe), tpes ::: rest)
                  case _             => inner(f(z, tpe), rest)
                }
            }
        }
      }
      inner(z, tpe :: Nil)
    }

    def [O, That](t1: Type) zipFold(t2: Type)(z: O)(f: (O, Type, Type) => O): O = {

      @tailrec
      def inner(z: O, args: List[Type], apps: List[Type]): O = args match {
        case Nil => z

        case arg :: argRest => apps match {
          case Nil => z

          case app :: appsRest =>
            if canUnify(arg, app) then (arg, app) match {
              case (AppliedType(_, tpes1), AppliedType(_, tpes2)) =>
                inner(
                  f(z, arg, app),
                  tpes1 ::: argRest,
                  tpes2 ::: appsRest
                )

              case (FunctionType(a1, b1), FunctionType(a2, b2)) =>
                inner(
                  f(z, arg, app),
                  a1 :: b1 :: argRest,
                  a2 :: b2 :: appsRest
                )

              case (LinearFunctionType(a1, b1), LinearFunctionType(a2, b2)) =>
                inner(
                  f(z, arg, app),
                  a1 :: b1 :: argRest,
                  a2 :: b2 :: appsRest
                )

              case (InfixAppliedType(_, a1, a2),
                InfixAppliedType(_, b1, b2)) =>
                  inner(
                    f(z, arg, app),
                    a1 :: a2 :: argRest,
                    b1 :: b2 :: appsRest
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

      if WildcardType == t2 || WildcardType == t1 then z
      else inner(z, t1 :: Nil, t2 :: Nil)
    }

    def [O, Col](ops: Type) zipWith(t: Type)(f: (Name, Type) => O)(using factory: Factory[O, Col]): Col = {

      val b = ops.zipFold(t)(factory.newBuilder) { (acc, arg, app) =>
        arg match {
          case Variable(name) => acc += f(name, app)
          case _              => acc
        }
      }

      b.result
    }

    def canUnify(arg: Type, app: Type) : Boolean = {
      def inner(z: Boolean, args: List[Type], apps: List[Type]): Boolean = args match {
        case Nil => z

        case arg :: argRest => apps match {
          case Nil => z

          case app :: appsRest => (arg, app) match {
            case (Product(tpes1), Product(tpes2)) => tpes1.size == tpes2.size
            case (Variable(_:Comp), _)            => app.isComputationType

            case (AppliedType(_, args1), AppliedType(_, args2)) =>
              args1.size == args2.size

            case ( _:FunctionType,       _:FunctionType       )
            |    ( _:LinearFunctionType, _:LinearFunctionType )
            |    ( _:InfixAppliedType,   _:InfixAppliedType   )
            |    ( _:BaseType,           _:BaseType           )
            |    ( _:TypeRef,            _:TypeRef            )
            |    ( _:Variable
                |    WildcardType,       _                    )
            |    ( _:PackageInfo,        _:PackageInfo        )
            |    (   EmptyType,            EmptyType          ) => true

            case _ => false
          }
        }
      }
      inner(true, arg :: Nil, app :: Nil)
    }

    def (tpe: Type) =:= (other: Type): Boolean = {
      tpe   == other        ||
      tpe   == WildcardType ||
      other == WildcardType
    }

    def isBootstrapped(name: Name) = bootstrapped.view.map(_._1).contains(name)

    def dataDefinitionName(tpe: Type): Name = {
      tpe match {
        case AppliedType(name, _) if !isBootstrapped(name)         => name
        case InfixAppliedType(name, _, _) if !isBootstrapped(name) => name
        case BaseType(name) if !isBootstrapped(name)               => name
        case _                                                     => EmptyName
      }
    }

    def (tpe: Type) unwrapLinearBody: Type = tpe match {
      case LinearFunctionType(_, ret) => ret
      case _                          => tpe
    }

    def unifies(tpe: Type, pt: Type) = {
      unifiesThen(tpe, pt)(_ => true)((_,_) => false)
    }

    def unifiesThen[U](tpe: Type, pt: Type)
                      (f: Type => U)
                      (orElse: (Type, Type) => U): U = {
      val tpe1 = tpe.unify(pt)
      val pt1  = pt.unify(tpe1)
      if pt1 =:= tpe1 then f(tpe1)
      else orElse(tpe1, pt1)
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

    def (t: Type) toCurriedList: List[Type] = {
      @tailrec
      def inner(acc: List[Type], t: Type): List[Type] = t match {
        case FunctionType(arg, body)  => inner(arg :: acc, body)
        case _                        => t :: acc
      }
      inner(Nil, t).reverse
    }

    def toBodyType(defSig: Tree, t: Type): Type =
      defSig match {
        case DefSig(_, args) =>
          toFunctionType(t.toCurriedList.drop(args.length))

        case LinearSig(_, args, _) =>
          val fArgs = t.toCurriedList.drop(args.length)
          fArgs.reverse match {
            case LinearFunctionType(arg1, body) :: Nil =>
              body
            case _ => toFunctionType(fArgs)
          }

        case _ => EmptyType
      }

    def toFunctionType(ts: List[Type]): Type = ts.reverse match {
      case t :: rest  => rest.foldLeft(t)((acc, t1) => FunctionType(t1, acc))
      case Nil        => EmptyType
    }

    def (subFrom: Type) unifications(subWith: Type): List[(Name, Type)] =
      subFrom.zipWith(subWith)((_, _))

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
      def inner(tpes: List[Type]): Boolean = tpes match {
        case Nil => true

        case tpe :: tpes => tpe match {
          case AppliedType(BangTag, _ :: Nil)
          |    InfixAppliedType(TensorTag,_,_)
          |    BaseType(VoidCompTag)
          |    TypeRef(_: Comp)
          |    Variable(_: Comp) => inner(tpes)

          case AppliedType(_, args) =>
            inner(args:::tpes)

          case InfixAppliedType(_, left, right) =>
            inner(left::right::tpes)

          case FunctionType(_, body) => inner(body::tpes)
          case Product(ts)           => inner(ts:::tpes)
          case _                     => false
        }
      }
      inner(tpe :: Nil)
    }

    def (tpe: Type) isValueType = !tpe.isComputationType

    private def (tpe: Type) showImpl: String = tpe.compile {
      case FunctionType(arg, body)        => fromFunctionType(arg, body)
      case LinearFunctionType(arg, body)  => fromLinearFunctionType(arg, body)
      case InfixAppliedType(op, a1, a2)   => fromInfixAppliedType(op,a1,a2)
      case AppliedType(f, args)           => fromAppliedType(f, args)
      case Product(tpes1)                 => fromProduct(tpes1)
      case TypeRef(t)                     => t.show::stack
      case BaseType(t)                    => t.show::stack
      case Variable(t)                    => t.show::stack
      case PackageInfo(parent, name)      => showPackage(parent, name)::stack
      case WildcardType                   => "<any>"::stack
      case EmptyType                      => "<nothing>"::stack
    }

    private def fromFunctionType(arg: Type, body: Type): Statement[String] = {
      val a1 :: a2 :: rest = stack
      val a1Final = arg match {
        case _: (FunctionType | LinearFunctionType)  => s"($a1)"
        case _                                       => a1
      }
      s"$a1Final -> $a2" :: rest
    }

    private def fromLinearFunctionType(arg: Type, body: Type): Statement[String] = {
      val a1 :: a2 :: rest = stack
      val a1Final = arg match {
        case _: (FunctionType | LinearFunctionType)  => s"($a1)"
        case _                                       => a1
      }
      s"$a1Final ->. $a2" :: rest
    }

    private def fromInfixAppliedType(op: Name, a1: Type, a2: Type): Statement[String] = {
      val b1 :: b2 :: rest = stack
      val b1Final = a1 match {
        case AppliedType(BangTag, _ :: Nil) => b1

        case _: (FunctionType | AppliedType | LinearFunctionType
        | InfixAppliedType) =>
          s"($b1)"

        case _ => b1
      }
      val b2Final = a2 match {
        case AppliedType(BangTag, _ :: Nil) => b2
        case _: (FunctionType | AppliedType | LinearFunctionType) => s"($b2)"
        case InfixAppliedType(op1,_,_) if op1 != op => s"($b2)"
        case _                                      => b2
      }
      s"$b1Final ${op.show} $b2Final" :: rest
    }

    private def fromAppliedType(f: Name, args: List[Type]): Statement[String] = {
      def checkAll(tpes: List[Type], strs: List[String]) =
        tpes.zip(strs).map(check)

      def check(tpe: Type, str: String) = tpe match {
        case _: (FunctionType | AppliedType | LinearFunctionType | InfixAppliedType) =>
          s"($str)"
        case _ =>
          str
      }

      val (removed, rest) = stack.splitAt(args.length)
      val str = {
        if removed.isEmpty then {
          f.show
        } else {
          val fStr = f.show
          val init =
            if f == BangTag && args.length == 1 then fStr
            else s"$fStr "
          checkAll(args, removed).mkString(init, " ", "")
        }
      }
      str :: rest
    }

    private def fromProduct(tpes: List[Type]): Statement[String] = {
      val (removed, rest) = stack.splitAt(tpes.length)
      removed.mkString("(", ", ", ")") :: rest
    }

    private inline def showPackage(parent: Type, name: Name): String = {
      (packageNames(parent) ::: name :: Nil)
        .map(_.show)
        .mkString("package ", ".", "")
    }

    private def displayString(tpe: Type) = {
      val body = tpe.showImpl
      val variableStrings = tpe.foldLeft(List.empty[String]) { (acc, t) =>
        t match {
          case t: Variable => t.showImpl :: acc
          case _           => acc
        }
      }.distinct.sorted
      val quantification = {
        Some(variableStrings)
          .filter(_.nonEmpty)
          .fold("")(_.mkString("forall ", " ", ". "))
      }
      s"$quantification$body"
    }

    given Show[Type] {

      val variables = {
        val alpha = LazyList('a' to 'z': _*)
        val numeric =
          for
            n <- LazyList.from(1)
            x <- alpha
          yield s"$x$n"
        alpha.map(_.toString) #::: numeric
      }

      def (xs: Seq[String]) filterVariables(ys: Seq[String]) =
        xs.filterNot(ys.contains)

      def (tpe: Type) toNames: (Seq[Name], Seq[Name]) = {
        tpe.foldLeft(List.empty[Name]) { (acc, t) =>
          t match {
            // case Variable(n)              => n::acc
            case TypeRef(n)               => n::acc
            case BaseType(n)              => n::acc
            case AppliedType(n,_)         => n::acc
            case InfixAppliedType(n,_,_)  => n::acc
            case _                        => acc
          }
        }.distinct.partition {
          case _: Comp => true
          case _       => false
        }
      }

      def (tpe: Type) fancyVariables: Type = {
        import NameOps._
        import Derived._
        val (comp, from) = tpe.toNames
        var stream = {
          variables
            .filterVariables(comp.map(_.show `replaceAll` ("#", "")))
            .filterVariables(from.map(_.show))
        }
        tpe.replaceVariables {
          case _:Comp =>
            val s = Comp(Str(stream.head + "#"))
            stream = stream.tail
            Some(s)
          case _:From =>
            val s = From(Str(stream.head))
            stream = stream.tail
            Some(s)
          case _ => None
        }
      }

      def (tpe: Type) show: String = {
        displayString(tpe.fancyVariables)
      }
    }

    given Define[Type] = displayString

    given Conversion[List[Type], Type] = {
      case tpe :: Nil => tpe
      case types      => Product(types)
    }

    given Conversion[Type, List[Type]] = {
      case Product(ls)  => ls
      case tpe          => tpe :: Nil
    }
  }
}
